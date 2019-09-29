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
module Common.Route where

import Data.Text (Text)

import Obelisk.Route
import Obelisk.Route.TH
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Sum
import Data.Functor.Identity
import Data.Some (Some)
import Data.List (nub)
import Data.Bifunctor (bimap)
import Data.Universe (universe, Universe)
import qualified Data.Some as Some

data Route :: * -> * where
  Route_Home :: Route ()
  Route_GetStarted :: Route ()
deriving instance Show (Route a)

deriveRouteComponent ''Route

backendRouteEncoder :: (check ~ Either Text) => Encoder check Identity (R (Sum Void1 (ObeliskRoute Route))) PageName
backendRouteEncoder = handleEncoder (\_ -> InR (ObeliskRoute_App Route_Home) :/ ()) $ pathComponentEncoder $ \case
  InL v -> case v of {}
  InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
    Route_Home -> PathEnd $ unitEncoder mempty
    Route_GetStarted -> PathSegment "get-started" $ unitEncoder mempty

-- | Provide a human-readable name for a given section
sectionTitle :: Some Route -> Text
sectionTitle (Some.This sec) = case sec of
  Route_Home -> "Home"
  Route_GetStarted -> "Get Started"

-- | Provide a human-readable name for a route
routeTitle :: R Route -> Text
routeTitle (sec :=> _) = sectionTitle $ Some.This sec

-- | Provide a human-readable description for a given section
sectionDescription :: Some Route -> Text
sectionDescription (Some.This sec) = case sec of
  Route_Home -> "Reflex: Practical Functional Reactive Programming"
  Route_GetStarted -> "Installation, Tutorials / Talks and FAQs"

-- | Provide a human-readable description for a given route
routeDescription :: R Route -> Text
routeDescription (sec :=> _) = sectionDescription $ Some.This sec

-- | Given a section, provide its default route
sectionHomepage :: Some Route -> R Route
sectionHomepage (Some.This sec) = sec :/ case sec of
  Route_Home -> ()
  Route_GetStarted -> ()
