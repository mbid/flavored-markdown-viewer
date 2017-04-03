module Inner(setInnerHTML, getInnerHTML, getInnerText) where

import Prelude

import DOM.HTML.Types (HTMLElement)
import DOM (DOM)
import Control.Monad.Eff (Eff)

foreign import setInnerHTML :: forall e.String -> HTMLElement -> Eff (dom :: DOM | e) Unit
foreign import getInnerHTML :: forall e. HTMLElement -> Eff (dom :: DOM | e) Unit
foreign import setInnerText :: forall e. String -> HTMLElement -> Eff (dom :: DOM | e) String
foreign import getInnerText :: forall e. HTMLElement -> Eff (dom :: DOM | e) String
