{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}

module Backend where

import Common.Route
import Obelisk.Route
import Obelisk.Backend
import Data.Dependent.Sum

backend :: Backend Void1 Route
backend = Backend
  { _backend_routeEncoder = backendRouteEncoder
  , _backend_run = \serve -> serve $ \(k :=> _) -> case k of {}
  }
