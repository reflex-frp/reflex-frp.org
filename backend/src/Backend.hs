{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Backend where

import Common.Route
import Data.Dependent.Sum (DSum (..))
import Obelisk.Backend

backend :: Backend BackendRoute Route
backend = Backend
  { _backend_routeEncoder = fullRouteEncoder
  , _backend_run = \serve -> serve $ \case
      r :=> _ -> case r of
        BackendRoute_Missing -> pure ()
  }
