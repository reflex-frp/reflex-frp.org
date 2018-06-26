{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Common.Route where

import Reflex.Dom

import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Maybe

import Obelisk.Route
import Data.Universe
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Dependent.Sum
import Data.GADT.Compare.TH
import Data.GADT.Show
import Data.GADT.Show.TH
import Data.Functor.Identity

--------------------------------------------------------------------------------
-- Useful
--------------------------------------------------------------------------------

data Route :: * -> * where
  Route_Home :: Route ()
  Route_Tutorials :: Route ()
  Route_Examples :: Route ()
  Route_Documentation :: Route ()
  Route_FAQ :: Route ()

--TODO: This naming convention should be changed
--TODO: showRoute and routeEncoder should be separate
showRoute :: Some Route -> Maybe Text
showRoute = \case
  Some.This Route_Home -> Nothing
  Some.This Route_Tutorials -> Just "tutorials"
  Some.This Route_Examples -> Just "examples"
  Some.This Route_Documentation -> Just "documentation"
  Some.This Route_FAQ -> Just "faq"

--------------------------------------------------------------------------------
-- Annoying
--------------------------------------------------------------------------------

--TODO: Don't use the term "rest" - it's loaded in web apps
--TODO: We really shouldn't have to write this
routeRestEncoder :: (MonadError Text check, MonadError Text parse) => Route a -> Encoder check parse a PageName
routeRestEncoder = Encoder . pure . \case --TODO: Shouldn't have to say `Encoder . pure` here
  Route_Home -> endValidEncoder mempty
  Route_Tutorials -> endValidEncoder mempty
  Route_Examples -> endValidEncoder mempty
  Route_Documentation -> endValidEncoder mempty
  Route_FAQ -> endValidEncoder mempty

deriving instance Show (Route a)

instance Universe (Some Route) where
  universe =
    [ Some.This Route_Home
    , Some.This Route_Tutorials
    , Some.This Route_Examples
    , Some.This Route_Documentation
    , Some.This Route_FAQ
    ]

deriveGCompare ''Route
deriveGEq ''Route
deriveGShow ''Route

instance EqTag Route Identity where
  eqTagged r _ a b = case r of
    Route_Home -> a == b
    Route_Tutorials -> a == b
    Route_Examples -> a == b
    Route_Documentation -> a == b
    Route_FAQ -> a == b

instance OrdTag Route Identity where
  compareTagged r _ a b = case r of
    Route_Home -> a `compare` b
    Route_Tutorials -> a `compare` b
    Route_Examples -> a `compare` b
    Route_Documentation -> a `compare` b
    Route_FAQ -> a `compare` b
