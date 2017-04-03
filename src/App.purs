module App (runApp) where

import Prelude
import GithubMarkdown as GM
import Halogen.HTML as HH
import Raw as Raw
import ShowdownMarkdown as SM
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.State.Class (gets, modify)
import Data.Array (fromFoldable, init, last, null, singleton, snoc)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.String (Pattern(..), split)
import GithubMarkdown (GithubApiError(..))
import Halogen (Component, ParentDSL, ParentHTML, lifecycleParentComponent)
import Halogen.Aff (awaitBody)
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.HTML (ClassName(..), HTML, article, br_, h3_, p_, slot, text)
import Halogen.HTML.Properties (class_, classes)
import Halogen.VDom.Driver (runUI)
import Inner (getInnerText, setInnerHTML)
import Network.HTTP.Affjax (AJAX)
import Partial.Unsafe (unsafePartial)
import Raw (rawHTML)
import Settings (Settings, SettingsOverride, emptyOverride, override)

type Effects e =
  ( console :: CONSOLE
  , ajax :: AJAX
  | HalogenEffects e
  )

type M e = (Aff (Effects e))

data Renderer = Github | Showdown

type State =
  { markdown :: String
  , html :: Maybe String
  , globalSettings :: Settings
  , localSettings :: SettingsOverride
  , renderError :: Maybe GithubApiError
  , renderer :: Maybe Renderer
  }

settings :: State -> Settings
settings s = override s.localSettings s.globalSettings

type Output = Void
type Input = Settings

data Query a = UpdateGlobalSettings a Settings
             | Rerender a

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
  , renderer: Nothing
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
      liftAff (GM.markdownToHtml auth markdown) >>= case _ of
        Right html -> do
          liftEff $ log "Github rendered"
          modify \s -> s
            { renderError = Nothing
            , html = Just html
            , renderer = Just Github
            }
        Left error -> do
          liftEff $ log "Showdown rendered"
          modify \s -> s
            { renderError = Just error
            , html = Just $ SM.markdownToHtml markdown
            , renderer = Just Showdown
            }
      pure next

intersperse :: forall a. a -> Array a -> Array a
intersperse x xs =
  if null xs
  then xs
  else 
    unsafePartial $ snoc
    (fromJust (init xs) >>= \x' -> [x', x])
    (fromJust (last xs))

multiline :: forall p i. String -> Array (HTML p i)
multiline = split (Pattern "\n") >>> map text >>> intersperse br_

warningBox :: forall p i. State -> Maybe (HTML p i)
warningBox s = case s.renderError of
  Nothing -> Nothing
  Just err ->
    Just $ HH.div [ class_ $ ClassName "warning-box" ] $
    errorDescription err <> renderNotice s.renderer
  where
    errorDescription :: GithubApiError -> Array (HTML p i)
    errorDescription RateLimited = [ h3_ [ text "Rate limit reached. Try again later." ] ]
    errorDescription (UnknownError err) =
      [ h3_ [ text $ "An unknown error with the github api has occured. Consider reporting this." ]
      , p_ [ text err ]
      ]
    errorDescription (AffjaxError err) =
      [ h3_ [ text $ "Could not reach the github servers. Are you offline?" ] ]

    renderNotice :: Maybe Renderer -> Array (HTML p i)
    renderNotice Nothing = []
    renderNotice (Just Showdown) = 
      [ p_ [ text "Showing an approximation to how it will look on github." ] ]
    renderNotice (Just Github) = 
      [ p_ [ text "This shouldn't be possible." ] ]

markdownBody :: forall e. State -> ParentHTML Query Raw.Query Slot (M e)
markdownBody s = 
  case s.html of
    Nothing -> article [ class_ $ ClassName "plain-body" ] $ multiline s.markdown
    Just html ->
      article
      [ class_ $ ClassName "markdown-body" ]
      [ slot RenderedSlot rawHTML html absurd ]

render :: forall e. State -> ParentHTML Query Raw.Query Slot (M e)
render s =
  HH.div [ class_ $ ClassName "app-container" ] $
  fromFoldable (warningBox s) <> [ markdownBody s ]

receiver :: Input -> Maybe (Query Unit)
receiver = UpdateGlobalSettings unit >>> Just

initializer :: Maybe (Query Unit)
initializer = Just $ Rerender unit
finalizer :: Maybe (Query Unit)
finalizer = Nothing

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
