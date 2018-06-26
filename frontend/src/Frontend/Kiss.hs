{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend.Kiss  where

import Reflex
import Reflex.Dom.Core

import Control.Monad.Fix
import Control.Monad

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid

import Frontend.FontAwesome as FA

import Common.Route

--IsPath Modules---
import Language.Javascript.JSaddle
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.Location as Location
import qualified GHCJS.DOM.History as History
import qualified GHCJS.DOM.WindowEventHandlers as DOM
import GHCJS.DOM.Types (Location)
import qualified GHCJS.DOM.Types as DOM
import Control.Lens
--------------------------------


