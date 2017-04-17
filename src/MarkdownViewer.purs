module MarkdownViewer
( markdownViewer
, MarkdownViewerQuery(Navigate)
, MarkdownViewerEffects
, MarkdownViewerMessage(..)
) where

import Prelude
import GithubMarkdown as GM
import Raw as Raw
import ShowdownMarkdown as SM
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.State (gets)
import Control.Monad.State.Class (state)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Tuple (Tuple(..))
import GithubMarkdown (GithubApiError, GithubAuth)
import Halogen (ClassName(..), Component, ParentDSL, ParentHTML, lifecycleParentComponent, raise)
import Halogen.HTML (HTML, article, slot)
import Halogen.HTML.Properties (class_)
import Network.HTTP.Affjax (AJAX)
import Raw (rawHTML)

foreign import scrollIntoView  :: forall e. Element -> Boolean -> Eff (dom :: DOM | e) Unit

type MarkdownViewerEffects e =
  ( console :: CONSOLE
  , ajax :: AJAX
  , dom :: DOM
  | e
  )
type Effects e = MarkdownViewerEffects e

type M e = Aff (Effects e)

data Renderer = Github | Showdown

type State =
  { markdown :: String
  , markdownVersion :: Int
  , html :: String
  , htmlVersion :: Int
  , auth :: Maybe GithubAuth
  }

data MarkdownViewerMessage = StartedRendering
                           | StoppedRendering
                           | RenderError GithubApiError
                           | RenderSuccess
type Message = MarkdownViewerMessage

type Input = String

data MarkdownViewerQuery a = SetMarkdown a String
                           | Rerender a
                           | Navigate a String
type Query = MarkdownViewerQuery

data Slot = RawSlot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type DSL e = ParentDSL State Query Raw.Query Slot Message (M e)

initialState :: Input -> State
initialState markdown =
  { markdown: markdown
  , markdownVersion: 1
  , html: SM.markdownToHtml markdown
  , htmlVersion: 0
  , auth: Nothing
  }

eval :: forall e. Query ~> DSL e
eval =
  case _ of
    SetMarkdown next markdown -> do
      changed <- state \s -> 
        if s.markdown /= markdown
        then
          Tuple true $
          s { markdown = markdown , markdownVersion = s.markdownVersion + 1 }
        else Tuple false s
      when changed $ eval $ Rerender unit
      pure next
    Rerender next -> do
      liftEff $ log "Rerendering"
      raise StartedRendering
      markdown <- gets \s -> s.markdown
      auth <- gets \s -> s.auth
      version <- gets \s -> s.markdownVersion
      liftAff (GM.markdownToHtml auth markdown) >>= case _ of
        Right html -> do
          liftEff $ log "Github rendered"
          updated <- state \s ->
            if s.htmlVersion <= version
            then Tuple true $ s { html = html , htmlVersion = version }
            else Tuple false s
          when updated $ raise $ RenderSuccess
        Left error -> do
          liftEff $ log "Showdown rendered"
          let html = SM.markdownToHtml markdown
          updated <- state \s ->
            if s.htmlVersion <= version
            then Tuple true $ s { html = html, htmlVersion = version }
            else Tuple false s
          when updated $ raise $ RenderError error
      finished <- gets \s -> s.markdownVersion <= s.htmlVersion
      when finished $ raise StoppedRendering
      pure next
    Navigate next fragment -> do
      let id = ElementId $ "user-content-" <> fragment
      doc <- liftEff $ window >>= (document >>> map htmlDocumentToDocument)
      n <- liftEff $ getElementById id (documentToNonElementParentNode doc)
      case toMaybe n of
        Nothing -> do
          liftEff $ log "not found"
          pure unit
        Just el -> liftEff $ scrollIntoView el true
      liftEff $ log fragment
      pure next

render :: forall e. State -> ParentHTML Query Raw.Query Slot (M e)
render s =
  article
  [ class_ $ ClassName "markdown-body" ]
  [ slot RawSlot rawHTML s.html absurd ]

receiver :: Input -> Maybe (Query Unit)
receiver = SetMarkdown unit >>> Just

initializer :: Maybe (Query Unit)
initializer = Just $ Rerender unit
finalizer :: Maybe (Query Unit)
finalizer = Nothing

markdownViewer :: forall e. Component HTML Query Input Message (M e)
markdownViewer = 
  lifecycleParentComponent 
  { initialState: initialState
  , render: render
  , eval: eval
  , receiver: receiver
  , initializer: initializer
  , finalizer: finalizer
  }
