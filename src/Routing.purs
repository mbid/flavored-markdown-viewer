module Routing (onHashChange, fragment) where

import Prelude
import Data.String as S
import Control.Monad.Aff (Aff, launchAff, liftEff')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (Event)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (hashchange)
import DOM.HTML.Event.Types (HashChangeEvent, readHashChangeEvent)
import DOM.HTML.Types (windowToEventTarget)
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Maybe (Maybe)

fragment :: String -> Maybe String
fragment = S.dropWhile (_ /= '#') >>> S.uncons >>> map _.tail

onHashChange :: forall e a.
  (HashChangeEvent -> Aff (dom :: DOM | e) a) -> Aff (dom :: DOM | e) Unit
onHashChange callback = do
  res <- liftEff' $
    window
      >>= windowToEventTarget
      >>> addEventListener hashchange (eventListener effCallback) false
  case res of
    Right x -> pure x
    Left err -> throwError err
  where
    effCallback :: Event -> Eff (dom :: DOM, err :: EXCEPTION | e) Unit
    effCallback ev =
      case runExcept (readHashChangeEvent (toForeign ev)) of
        Left _ -> pure unit
        Right hcev -> do
          launchAff $ callback hcev
          pure unit
