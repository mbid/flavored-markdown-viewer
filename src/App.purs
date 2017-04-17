module App (runApp) where

import Prelude
import Halogen.HTML as HH
import MarkdownViewer as MarkdownViewer
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Wait (wait)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Fork.Class (fork)
import Control.Monad.Rec.Loops (whileM_)
import Control.Monad.State (gets, modify)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Event.HashChangeEvent (newURL)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Document (url)
import Data.Array (singleton)
import Data.Maybe (Maybe(..), isJust, maybe)
import GithubMarkdown (GithubApiError(..))
import Halogen (ClassName(..), Component, ParentDSL, ParentHTML, lifecycleParentComponent, query)
import Halogen.Aff (awaitBody)
import Halogen.HTML (AttrName(..), HTML, attr, br_, button, i, pre_, slot, text)
import Halogen.HTML.Elements (h4_)
import Halogen.HTML.Properties (class_, classes, id_, tabIndex)
import Halogen.HTML.Properties.ARIA (role)
import Halogen.Themes.Bootstrap3 (alert, alertDanger, alertWarning, close, fade, modal, modalBody, modalContent, modalDialog, modalHeader, textWarning)
import Halogen.VDom.Driver (runUI)
import Inner (getInnerText, setInnerHTML)
import MarkdownViewer (MarkdownViewerEffects, MarkdownViewerMessage(..), MarkdownViewerQuery, markdownViewer)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Network.HTTP.StatusCode (StatusCode(..))
import Routing (fragment, onHashChange)
import Settings (Settings, SettingsOverride, emptyOverride, override, defaultSettings)

type AppEffects e = (timer :: TIMER | MarkdownViewerEffects e)
type Effects e = AppEffects e

type M e = Aff (Effects e)

type State =
  { markdown :: String
  , sourceUrl :: String
  , globalSettings :: Settings
  , localSettings :: SettingsOverride
  , renderError :: Maybe GithubApiError
  , rendering :: Boolean
  , alive :: Boolean
  }

settings :: State -> Settings
settings s = override s.localSettings s.globalSettings

type AppMessage = Void
type Message = AppMessage

type AppInput =
  { sourceUrl :: String
  , globalSettings :: Settings
  }
type Input = AppInput

data AppQuery a = Initialize a
                | Finalize a
                | Navigate a String
                | Receive a MarkdownViewerMessage
type Query = AppQuery

data Slot = MarkdownSlot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type ChildQuery = MarkdownViewerQuery

type DSL e = ParentDSL State Query ChildQuery Slot Message (M e)

initialState :: String -> Input -> State
initialState markdown input =
  { markdown: markdown
  , sourceUrl: input.sourceUrl
  , globalSettings: input.globalSettings
  , localSettings: emptyOverride
  , renderError: Nothing
  , rendering: false
  , alive: true
  }

reloadDaemon :: forall e. DSL e Unit
reloadDaemon = do
  whileM_ (gets _.alive) do
    url <- gets \s -> s.sourceUrl
    resp <- liftAff $ affjax $ defaultRequest { url = url }
    if resp.status /= StatusCode 200 && resp.status /= StatusCode 0
      then liftEff $ log $ "Error reloading: " <> show resp.status -- TODO
      else modify \s -> s { markdown = resp.response }
    liftEff $ log "Reloaded"
    gets (settings >>> _.reloadInterval) >>= (wait >>> liftAff)


eval :: forall e. Query ~> DSL e
eval =
  case _ of
    Initialize next -> do
      liftEff $ log "Reloading"
      fork reloadDaemon
      pure next
    Finalize next -> do
      modify \s -> s { alive = false }
      pure next
    Navigate next frag -> do
      liftEff $ log $  "Navigate to " <> frag
      query MarkdownSlot $ MarkdownViewer.Navigate unit frag
      pure next
    Receive next StartedRendering -> do
      modify \s -> s { rendering = true }
      pure next
    Receive next StoppedRendering -> do
      modify \s -> s { rendering = false }
      pure next
    Receive next (RenderError err) -> do
      modify \s -> s { renderError = Just err }
      pure next
    Receive next RenderSuccess -> do
      modify \s -> s { renderError = Nothing }
      pure next

errorModalId :: String
errorModalId = "error-modal"

modalCloseButton :: forall p i. HTML p i
modalCloseButton = 
  button
  [ attr (AttrName "type") "button"
  , classes [ close ]
  , attr (AttrName "data-dismiss") "modal"
  , attr (AttrName "aria-label") "Close"
  ]
  [ text "×" ]

renderErrorModalContent :: forall p i. GithubApiError -> HTML p i
renderErrorModalContent = case _ of
  RateLimited ->
    HH.div [ classes [ modalContent, alert, alertWarning ] ]
    [ HH.div [ classes [ modalHeader ] ] 
      [ modalCloseButton , h4_ [ text "Rate limit reached" ]
    ]
    , HH.div [ classes [ modalBody ] ]
      [ text 
        """
        Rate limit of the GitHub servers reached.
        Try again later to use GitHub's original markdown renderer.
        """
      , br_
      , text "Showing a close approximation for now."
      ]
    ]
  UnknownError err ->
    HH.div [ classes [ modalContent, alert, alertDanger ] ]
    [ HH.div [ classes [ modalHeader ] ]
      [ modalCloseButton, h4_ [ text "Unknown error" ] ]
    , HH.div [ classes [ modalBody ] ]
      [ text
        """
        An unknown error with the GitHub api has occured.
        Consider reporting this.
        """
      , pre_ [ text err ]
      ]
    ]
  AffjaxError err ->
    HH.div [ classes [ modalContent, alert, alertWarning ] ]
    [ HH.div [ classes [ modalHeader ] ]
      [ modalCloseButton, h4_ [ text "Could not reach the GitHub servers" ] ]
    , HH.div [ classes [ modalBody ] ] [ text "Are you offline?" ]
    ]

renderErrorModal :: forall p i. State -> HTML p i
renderErrorModal s =
  HH.div
  [ classes [ modal, fade ]
  , id_ errorModalId
  , tabIndex (-1)
  , role "dialog"
  ]
  [ HH.div [ classes [ modalDialog ], role "document" ]
    [ maybe (HH.div_ []) renderErrorModalContent s.renderError ]
  ]

iconButton :: ClassName
iconButton = ClassName "icon-button"

globalMenu :: ClassName
globalMenu = ClassName "menu"

renderMenu :: forall p i. State -> HTML p i
renderMenu s =
  HH.div [ classes [ globalMenu ] ] $
  ( if s.rendering
    then singleton $
      i [ classes $ map ClassName [ "fa", "fa-refresh", "fa-spin", "fa-2x", "fa-fw" ] ] []
    else []
  )
  <>
  ( if isJust s.renderError
    then singleton $
      button
      [ classes [ iconButton ]
      , attr (AttrName "data-toggle") "modal"
      , attr (AttrName "data-target") $ "#" <> errorModalId
      ]
      [ i 
        [ classes $ [ textWarning ] <>
          map ClassName [ "fa", "fa-fw", "fa-2x", "fa-exclamation-triangle"]
        ]
        []
      ]
    else []
  )

render :: forall e. State -> ParentHTML Query ChildQuery Slot (M e)
render s =
  HH.div [ class_ $ ClassName "app-container" ] $
  [ renderMenu s
  , renderErrorModal s
  , slot MarkdownSlot markdownViewer s.markdown $
    Receive unit >>> Just
  ]

receiver :: Input -> Maybe (Query Unit)
receiver = const Nothing

initializer :: Maybe (Query Unit)
initializer = Just $ Initialize unit
finalizer :: Maybe (Query Unit)
finalizer = Nothing

app :: forall e. String -> Component HTML Query Input Message (M e)
app initialMarkdown = 
  lifecycleParentComponent 
  { initialState: initialState initialMarkdown
  , render: render
  , eval: eval
  , receiver: receiver
  , initializer: initializer
  , finalizer: finalizer
  }

runApp :: forall t31.
  Aff
    ( dom :: DOM
    , avar :: AVAR
    , timer :: TIMER
    , ref :: REF
    , err :: EXCEPTION
    , console :: CONSOLE
    , ajax :: AJAX
    | t31
    )
    Unit
runApp = do
  body <- awaitBody
  markdown <- liftEff $ getInnerText body
  -- an easier way for this?
  sourceUrl <- liftEff $ window >>= (document >>> map htmlDocumentToDocument) >>= url
  liftEff $ setInnerHTML "" body
  ui <-
    runUI (app markdown)
    { sourceUrl: sourceUrl, globalSettings: defaultSettings }
    body
  onHashChange 
    \hcev -> case fragment $ newURL hcev of
      Nothing -> pure unit
      Just frag -> do
        liftEff $ log "onHashChange"
        ui.query $ Navigate unit frag
    
  pure unit
  liftEff $ log "App running!"
