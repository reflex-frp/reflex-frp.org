{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.Link where

import Prelude hiding ((.))

import Control.Category ((.))
import Control.Lens ((%~))
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core

import Common.Route
import Obelisk.Route
import Obelisk.Route.Frontend

-- | A link widget that, when clicked, sets the route to the provided route. In non-javascript
-- contexts, this widget falls back to using @href@s to control navigation
routedLink
  :: forall t m a.
     ( DomBuilder t m
     , SetRoute t (R Route) m
     )
  => R Route -- ^ Target route
  -> m a -- ^ Child widget
  -> m a
routedLink r w = do
  let Right enc = checkEncoder backendRouteEncoder
      p = encode (pageNameEncoder :: Encoder Identity (Either Text) PageName PathQuery)
      toPathQuery :: R Route -> PathQuery = \(k :/ v) -> p . encode enc $ InR (ObeliskRoute_App k) :/ v
      (path, query) = toPathQuery r
      cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
        & elementConfig_initialAttributes .~ "href" =: T.pack (path <> query)
  (e, a) <- element "a" cfg w
  setRoute $ r <$ domEvent Click e
  return a
