module App (runApp) where

import Prelude
import Halogen.HTML as HH
import Raw as Raw
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Wait (wait)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Rec.Loops (whileM_)
import Control.Monad.State.Class (gets, modify)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Document (url)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Time.Duration (Seconds(..))
import GithubMarkdown (GithubApiError, markdownToHtml)
import Halogen (Component, ParentDSL, ParentHTML, lifecycleParentComponent)
import Halogen.Aff (awaitBody)
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.HTML (ClassName(..), HTML(..), div, slot, text)
import Halogen.HTML.Properties (class_)
import Halogen.VDom.Driver (runUI)
import Inner (getInnerText, setInnerHTML)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Network.HTTP.StatusCode (StatusCode(..))
import Raw (rawHTML)
import Settings (Settings, SettingsOverride, emptyOverride, override)

type Effects e =
  ( console :: CONSOLE
  , ajax :: AJAX
  , timer :: TIMER
  | HalogenEffects e
  )

type M e = (Aff (Effects e))

type State =
  { markdown :: String
  , html :: Maybe String
  , globalSettings :: Settings
  , localSettings :: SettingsOverride
  , renderError :: Maybe GithubApiError
  , reloadError :: Maybe Unit
  , isAlive :: Boolean
  }

settings :: State -> Settings
settings s = override s.localSettings s.globalSettings

type Output = Void
type Input = Settings

data Query a = UpdateGlobalSettings a Settings
             | Reload a
             | Rerender a
             | Initialize a
             | Finalize a

data Slot = UnrenderedSlot
          | RenderedSlot
derive instance eqButtonSlot :: Eq Slot
derive instance ordButtonSlot :: Ord Slot

type DSL e = ParentDSL State Query Raw.Query Slot Output (M e)

initialState :: String -> Input -> State
initialState markdown settings' =
  { markdown: markdown
  , html: Nothing
  , globalSettings: settings'
  , localSettings: emptyOverride
  , renderError: Nothing
  , reloadError: Nothing
  , isAlive: true
  }

eval :: forall e. Query ~> DSL e
eval =
  case _ of
    UpdateGlobalSettings next settings' -> do
      modify \s -> s { globalSettings = settings' }
      pure next
    Rerender next -> do
      liftEff $ log "Rerendering"
      markdown <- gets \s -> s.markdown
      auth <- gets \s -> (settings s).auth
      liftAff (markdownToHtml auth markdown) >>= case _ of
        Left error -> do
          modify \s -> s { renderError = Just error }
        Right html -> do
          modify \s -> s { renderError = Nothing, html = Just html }
      pure next
    Reload next -> do
      url <- liftEff $ window >>= (document >>> map htmlDocumentToDocument) >>= url
      resp <- (liftAff $ affjax $ defaultRequest { url = url })
      if resp.status /= StatusCode 200 && resp.status /= StatusCode 0
        then modify \s -> s { reloadError = Just unit } -- todo
        else do
          liftEff $ log "Reloaded"
          let markdown = resp.response
          oldMarkdown <- gets \s -> s.markdown
          oldHtml <- gets \s -> s.html
          modify \s -> s { reloadError = Nothing, markdown = resp.response }
          when (oldMarkdown /= markdown || isNothing oldHtml) $ eval $ Rerender unit
      pure next
    Initialize next -> do
      eval $ Rerender unit
      eval $ Reload unit
      whileM_ (gets _.isAlive) do
        wait $ Seconds 0.2
        eval $ Reload unit
      pure next
    Finalize next -> do
      modify \s -> s { isAlive = false }
      pure next

render :: forall e. State -> ParentHTML Query Raw.Query Slot (M e)
render s = case s.html of
  Nothing -> text s.markdown
  Just html ->
    HH.div [ class_ $ ClassName "markdown-body" ]
    [ slot RenderedSlot rawHTML html absurd ]

receiver :: Input -> Maybe (Query Unit)
receiver = UpdateGlobalSettings unit >>> Just

initializer :: Maybe (Query Unit)
initializer = Just $ Initialize unit
finalizer :: Maybe (Query Unit)
finalizer = Just $ Finalize unit

app :: forall e. String -> Component HTML Query Input Output (M e)
app initialMarkdown = 
  lifecycleParentComponent 
  { initialState: initialState initialMarkdown
  , render: render
  , eval: eval
  , receiver: receiver
  , initializer: initializer
  , finalizer: finalizer
  }

runApp :: forall e. Aff (Effects e) Unit
runApp = do
  body <- awaitBody
  markdown <- liftEff $ getInnerText body
  liftEff $ setInnerHTML "" body
  runUI (app markdown) { auth: Nothing } body
  liftEff $ log "App running!"
  pure unit
