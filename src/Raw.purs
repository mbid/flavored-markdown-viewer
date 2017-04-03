module Raw (rawHTML, Query) where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import Inner (setInnerHTML)
import Data.Maybe (Maybe(..))
import Halogen.Query.InputF (RefLabel(..))

import Control.Monad.Eff.Console (CONSOLE, log)

--foreign import setHTML :: forall e. HTMLElement -> String -> Eff (dom :: DOM | e) Unit

type State = String

data Query a = Redraw a
             | Change a String

type Input = String
type Output = Void
type M e = (Aff (dom :: DOM, console :: CONSOLE | e))
type DSL e = H.ComponentDSL State Query Output (M e)

initialState :: Input -> State
initialState = id

label :: RefLabel
label = RefLabel "raw"

render :: State -> H.ComponentHTML Query
render = const $ HH.div [HP.ref label] []

eval :: forall e. Query ~> DSL e
eval = case _ of
  Redraw next -> do
    el <- H.getHTMLElementRef label >>= case _ of
      Nothing -> unsafeThrow "Element not rendered"
      Just el -> pure el
    htmlSrc <- H.get
    liftEff $ setInnerHTML htmlSrc el
    liftEff $ log "Redrawing"
    pure next
  Change next htmlSrc -> do
    prevHtmlSrc <- H.get
    when (prevHtmlSrc /= htmlSrc) do
      H.put htmlSrc
      eval $ Redraw unit
    pure next

receiver :: String -> Maybe (Query Unit)
receiver = Change unit >>> Just

initializer :: Maybe (Query Unit)
initializer = Just $ Redraw unit

finalizer :: Maybe (Query Unit)
finalizer = Nothing

rawHTML :: forall e. H.Component HH.HTML Query Input Output (M e)
rawHTML = 
  H.lifecycleComponent
  { initialState: initialState
  , render: render
  , eval: eval
  , receiver: receiver
  , initializer: initializer
  , finalizer: finalizer
  }
