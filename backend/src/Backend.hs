{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
module Backend where

import Common.Route
import Data.Dependent.Sum (DSum (..))
import Obelisk.Backend
import Obelisk.Route

backend :: Backend Void1 Route
backend = Backend
  { _backend_routeEncoder = backendRouteEncoder
  , _backend_run = \serve -> serve $ \case
      r :=> _ -> case r of {}
  }
