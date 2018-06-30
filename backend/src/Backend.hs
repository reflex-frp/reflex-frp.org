module Backend where

import Common.Route
import Obelisk.Route
import Frontend
import qualified Obelisk.Backend as Ob

backend :: IO ()
backend = Ob.backend (obeliskRouteEncoder routeComponentEncoder routeRestEncoder) Ob.def
  { Ob._backendConfig_head = fst frontend
  }
