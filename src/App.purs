module App (runApp) where

import Prelude
import Halogen.HTML as HH
import Raw as Raw
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.State.Class (gets, modify)
import Data.Array (last, null, snoc, tail)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.String (Pattern(..), split)
import GithubMarkdown (GithubApiError, markdownToHtml)
import Halogen (Component, ParentDSL, ParentHTML, lifecycleParentComponent)
import Halogen.Aff (awaitBody)
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.HTML (ClassName(..), HTML, br_, slot, text)
import Halogen.HTML.Properties (class_)
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

type State =
  { markdown :: String
  , html :: Maybe String
  , globalSettings :: Settings
  , localSettings :: SettingsOverride
  , renderError :: Maybe GithubApiError
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

intersperse :: forall a. a -> Array a -> Array a
intersperse x xs =
  if null xs
  then xs
  else 
    unsafePartial $ snoc
    (fromJust (tail xs) >>= \x' -> [x', x])
    (fromJust (last xs))

multiline :: forall p i. String -> Array (HTML p i)
multiline = split (Pattern "\n") >>> map text >>> intersperse br_

render :: forall e. State -> ParentHTML Query Raw.Query Slot (M e)
render s =
  HH.div [ class_ $ ClassName "markdown-body" ] $
  case s.html of
    Nothing -> multiline s.markdown
    Just html -> [ slot RenderedSlot rawHTML html absurd ]

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
