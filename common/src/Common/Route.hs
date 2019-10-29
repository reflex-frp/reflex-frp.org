{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

import Data.Text (Text)

import Obelisk.Route
import Obelisk.Route.TH
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Data.Some (Some)
import qualified Data.Some as Some

data BackendRoute :: * -> * where
  BackendRoute_Missing :: BackendRoute ()

data Route :: * -> * where
  Route_Home :: Route ()
  Route_GetStarted :: Route ()
  Route_Tutorial :: Route ()
  Route_Resources :: Route ()
deriving instance Show (Route a)

deriveRouteComponent ''Route
deriveRouteComponent ''BackendRoute

fullRouteEncoder :: (check ~ Either Text) => Encoder check Identity (R (FullRoute BackendRoute Route)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
    BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty)
  (\case
    Route_Home -> PathEnd $ unitEncoder mempty
    Route_GetStarted -> PathSegment "get-started" $ unitEncoder mempty
    Route_Tutorial -> PathSegment "tutorial" $ unitEncoder mempty
    Route_Resources -> PathSegment "resources" $ unitEncoder mempty)

-- | Provide a human-readable name for a given section
sectionTitle :: Some Route -> Text
sectionTitle (Some.Some sec) = case sec of
  Route_Home -> "Home"
  Route_GetStarted -> "Get Started"
  Route_Tutorial -> "Tutorial"
  Route_Resources -> "Resources"

-- | Provide a human-readable name for a route
routeTitle :: R Route -> Text
routeTitle (sec :=> _) = sectionTitle $ Some.Some sec

-- | Given a section, provide its default route
sectionHomepage :: Some Route -> R Route
sectionHomepage (Some.Some sec) = sec :/ case sec of
  Route_Home -> ()
  Route_GetStarted -> ()
  Route_Tutorial -> ()
  Route_Resources -> ()
