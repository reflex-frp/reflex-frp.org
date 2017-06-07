{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Router where 

import Reflex.Dom
import Data.Text (Text)
import Language.Javascript.JSaddle
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.Location as Location
import qualified GHCJS.DOM.History as History
import qualified GHCJS.DOM.WindowEventHandlers as DOM
import Language.Javascript.JSaddle (freeFunction, function, js, js0, js1, js2, jsg, jsg1, toJSVal, fromJSVal, fromJSValUnchecked)
import qualified Language.Javascript.JSaddle as JS
import GHCJS.DOM.Types (Location)
import qualified GHCJS.DOM.Types as DOM
import Control.Monad
import Control.Lens
import Control.Monad.Fix
import Focus.JS.Prerender

class IsPath path where 
  pathToText :: path -> Text
  textToPath :: Text -> path


browserHistoryPath :: (IsPath p, MonadJSM m, TriggerEvent t m, Reflex t) => m (p, Event t p) 
browserHistoryPath = do
  (t0, tE) <- browserHistory
  return (textToPath t0, fmap textToPath tE)

routeSwitch :: (IsPath b, DomBuilder t m, Prerender x m, TriggerEvent t m, PerformEvent t m, MonadHold t m, MonadFix m) 
            => b -> (b -> m (Event t b, a)) -> m (Dynamic t a)
routeSwitch initRoute f = do
        (r0, rEvent) <- prerender (return (initRoute, never)) browserHistoryPath
        let pagefor t = Workflow $ do
                      (switchE, x) <- f t 
                      prerender (return ()) ( performEvent_ . ffor switchE $ \t' -> pushState' (pathToText t')) 
                      return (x, fmap pagefor (leftmost [switchE, rEvent])) 
        workflow (pagefor r0)

browserHistory :: (MonadJSM m, TriggerEvent t m) => m (Text , Event t Text)
browserHistory = do
  window <- DOM.currentWindowUnchecked
  location <- Window.getLocation window
  loc0 <- getPathDecoded location
  loc <- wrapDomEvent window (`DOM.on` DOM.popState) $ getPathDecoded location
  return (loc0, loc) 

-- | Wrapper for pushState that doesn't require state or title.
pushState' :: MonadJSM m => Text -> m ()
pushState' r = do
  history <- Window.getHistory =<< DOM.currentWindowUnchecked
  History.pushState history () (""::Text) $ Just r

-- | Return the appended path, sarch and hash excluding hostname or port.
getPath :: MonadJSM m => Location -> m Text
getPath location = do
  pathname <- Location.getPathname location
  search <- Location.getSearch location
  hash <- Location.getHash location
  return $ mconcat [pathname, search, hash]

-- | encodeURI is used for full urls and components only for segments like search, hash, or path
getPathDecoded :: MonadJSM m => Location -> m Text
getPathDecoded = decodeURI <=< getPath

-- FIXME decodeURI may throw
decodeURI :: (MonadJSM m, ToJSString a, FromJSString b) => a -> m b
decodeURI input = do
  window <-  DOM.currentWindowUnchecked
  window' <- DOM.liftJSM $ toJSVal window
  DOM.liftJSM $ window' ^. js1 ("decodeURI"::Text) input >>= fromJSValUnchecked

-- FIXME decodeURIComponent may throw
decodeURIComponent :: (MonadJSM m, ToJSString a, FromJSString b) => a -> m b
decodeURIComponent input = do
  window <-  DOM.currentWindowUnchecked
  window' <- DOM.liftJSM $ toJSVal window
  DOM.liftJSM $ window' ^. js1 ("decodeURIComponent"::Text) input >>= fromJSValUnchecked
