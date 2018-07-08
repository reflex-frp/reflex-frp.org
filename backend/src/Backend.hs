module Backend where

import Common.Route
import Obelisk.Route
import Frontend
import qualified Obelisk.Backend as Ob

backend :: IO ()
backend = Ob.backend $ Ob.def
  { Ob._backendConfig_head = fst frontend
  , Ob._backendConfig_routeEncoder = obeliskRouteEncoder routeComponentEncoder routeRestEncoder
  }
