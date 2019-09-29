{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Frontend.Page.GetStarted (getStarted) where

import Control.Monad (forM_)
import Control.Monad.Fix
import Data.Dependent.Sum (DSum(..))
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Text (Text)
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Reflex.Dom
import Frontend.FontAwesome
import Frontend.CommonWidgets

import Common.Route

getStarted
  :: DomBuilder t m
  => m ()
getStarted = do
  el "h1" $ text "Getting Started"
