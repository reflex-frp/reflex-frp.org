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

import Prelude hiding ((.))

import Control.Monad.Except
import Data.Text (Text)

import Obelisk.Route
import Obelisk.Route.TH
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Sum
import Data.Functor.Identity
import Data.Some (Some)
import qualified Data.Some as Some

data Route :: * -> * where
  Route_Home :: Route ()
  Route_Tutorials :: Route ()
  Route_Examples :: Route ()
  Route_Documentation :: Route ()
  Route_FAQ :: Route ()
deriving instance Show (Route a)
deriveRouteComponent ''Route

backendRouteEncoder :: MonadError Text check => Encoder check Identity (R (Sum Void1 (ObeliskRoute Route))) PageName
backendRouteEncoder = handleEncoder (\_ -> InR (ObeliskRoute_App Route_Home) :/ ()) $ pathComponentEncoder $ \case
  InL v -> case v of {}
  InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
    Route_Home -> PathEnd $ unitEncoder mempty
    Route_Tutorials -> PathSegment "tutorials" $ unitEncoder mempty
    Route_Examples -> PathSegment "examples" $ unitEncoder mempty
    Route_Documentation -> PathSegment "documentation" $ unitEncoder mempty
    Route_FAQ -> PathSegment "faq" $ unitEncoder mempty

-- | Provide a human-readable name for a given section
sectionTitle :: Some Route -> Text
sectionTitle (Some.This sec) = case sec of
  Route_Home -> "Home"
  Route_Tutorials -> "Tutorials"
  Route_Examples -> "Examples"
  Route_Documentation -> "Documentation"
  Route_FAQ -> "FAQ"

routeTitle :: R Route -> Text
routeTitle (sec :=> _) = sectionTitle $ Some.This sec

-- | Given a section, provide its default route
sectionHomepage :: Some Route -> R Route
sectionHomepage (Some.This sec) = sec :/ case sec of
  Route_Home -> ()
  Route_Tutorials -> ()
  Route_Examples -> ()
  Route_Documentation -> ()
  Route_FAQ -> ()
