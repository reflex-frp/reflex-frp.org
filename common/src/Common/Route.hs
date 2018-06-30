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

import Control.Monad.Except
import Data.Text (Text)

import Obelisk.Route
import Data.Universe
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Dependent.Sum
import Data.GADT.Compare.TH
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
routeComponentEncoder :: (MonadError Text check, MonadError Text parse) => Encoder check parse (Some Route) (Maybe Text)
routeComponentEncoder = enum1Encoder $ \case
  Route_Home -> Nothing
  Route_Tutorials -> Just "tutorials"
  Route_Examples -> Just "examples"
  Route_Documentation -> Just "documentation"
  Route_FAQ -> Just "faq"

--------------------------------------------------------------------------------
-- Annoying
--------------------------------------------------------------------------------

--TODO: Don't use the term "rest" - it's loaded in web apps
--TODO: We should consider using a typeclass to infer the continuations for each thing
routeRestEncoder :: (MonadError Text check, MonadError Text parse) => Route a -> Encoder check parse a PageName
routeRestEncoder = Encoder . pure . \case --TODO: Shouldn't have to say `Encoder . pure` here
  Route_Home -> endValidEncoder mempty
  Route_Tutorials -> endValidEncoder mempty
  Route_Examples -> endValidEncoder mempty
  Route_Documentation -> endValidEncoder mempty
  Route_FAQ -> endValidEncoder mempty

deriving instance Show (Route a)

--TODO: Eliminate the need to write these instances by hand
instance Universe (Some Route) where
  universe =
    [ Some.This Route_Home
    , Some.This Route_Tutorials
    , Some.This Route_Examples
    , Some.This Route_Documentation
    , Some.This Route_FAQ
    ]

--TODO: Eliminate the need to write these instances by hand
instance EqTag Route Identity where
  eqTagged r _ a b = case r of
    Route_Home -> a == b
    Route_Tutorials -> a == b
    Route_Examples -> a == b
    Route_Documentation -> a == b
    Route_FAQ -> a == b

--TODO: Eliminate the need to write these instances by hand
instance OrdTag Route Identity where
  compareTagged r _ a b = case r of
    Route_Home -> a `compare` b
    Route_Tutorials -> a `compare` b
    Route_Examples -> a `compare` b
    Route_Documentation -> a `compare` b
    Route_FAQ -> a `compare` b

--TODO: Eliminate the need to write these instances by hand
instance ShowTag Route Identity where
  showTaggedPrec = \case
    Route_Home -> showsPrec
    Route_Tutorials -> showsPrec
    Route_Examples -> showsPrec
    Route_Documentation -> showsPrec
    Route_FAQ -> showsPrec

deriveGCompare ''Route
deriveGEq ''Route
deriveGShow ''Route
