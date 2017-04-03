module GithubMarkdownViewer (githubMarkdownViewer, Query(..)) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.Query as HQ
import Halogen.HTML as HH
import Raw as Raw
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Network.HTTP.Affjax (AJAX)
import Data.Either (Either(..))

import DOM (DOM)

import Control.Monad.Eff.Console (CONSOLE)
import GithubMarkdown (markdownToHtml, GithubAuth, GithubApiError)

type State =
  { markdown :: String
  , auth :: Maybe GithubAuth
  , renderedHtml :: Maybe String
  , version :: Int
  }
type Output = GithubApiError
type Input = String

data Query a = SetMarkdown a String
             | Rerender a

data Slot = MarkdownSlot
derive instance eqButtonSlot :: Eq Slot
derive instance ordButtonSlot :: Ord Slot

type M e = (Aff (dom :: DOM, console :: CONSOLE, ajax :: AJAX | e))
type DSL e = H.ParentDSL State Query Raw.Query Slot Output (M e)

initialState :: Input -> State
initialState markdown =
  { markdown: markdown
  , renderedHtml: Nothing
  , version: 0
  , auth: Nothing
  }

eval :: forall e. Query ~> DSL e
eval = case _ of
  SetMarkdown next markdown -> do
    version <- H.gets $ \s -> 1 + s.version
    auth <- H.gets $ \s -> s.auth
    html <- liftAff (markdownToHtml auth markdown)
    case html of
      Left error -> HQ.raise error
      Right html' -> H.modify $ \s -> s { markdown = markdown, renderedHtml = Just html', version = version }
    pure next
  Rerender next -> do
    markdown <- H.gets $ \s -> s.markdown
    eval $ SetMarkdown unit markdown
    pure next
render :: forall e. State -> H.ParentHTML Query Raw.Query Slot (M e)
render s =
  HH.slot MarkdownSlot
  Raw.rawHTML (fromMaybe s.markdown s.renderedHtml)
  absurd

receiver :: Input -> Maybe (Query Unit)
receiver = SetMarkdown unit >>> Just

githubMarkdownViewer :: forall e. H.Component HH.HTML Query Input Output (M e)
githubMarkdownViewer = 
  H.lifecycleParentComponent 
  { initialState: initialState
  , render: render
  , eval: eval
  , receiver: receiver
  , initializer: Just $ Rerender unit
  , finalizer: Nothing
  }
