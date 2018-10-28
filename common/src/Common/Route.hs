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
  Route_Talks :: Route (Maybe (R Talk))
  Route_FAQ :: Route ()
deriving instance Show (Route a)

data Talk :: * -> * where
  Talk_PracticalFRP :: Talk (R PracticalFRP)
  Talk_RealWorld :: Talk ()
  Talk_BrowserProgramming :: Talk ()
  Talk_Cochleagram :: Talk ()
  Talk_ReflexDomWithCss :: Talk ()
deriving instance Show (Talk a)

data PracticalFRP :: * -> * where
  PracticalFRP_Part1 :: PracticalFRP ()
  PracticalFRP_Part2 :: PracticalFRP ()
deriving instance Show (PracticalFRP a)

deriveRouteComponent ''Route
deriveRouteComponent ''Talk
deriveRouteComponent ''PracticalFRP

backendRouteEncoder :: (check ~ Either Text) => Encoder check Identity (R (Sum Void1 (ObeliskRoute Route))) PageName
backendRouteEncoder = handleEncoder (\_ -> InR (ObeliskRoute_App Route_Home) :/ ()) $ pathComponentEncoder $ \case
  InL v -> case v of {}
  InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
    Route_Home -> PathEnd $ unitEncoder mempty
    Route_Tutorials -> PathSegment "tutorials" $ unitEncoder mempty
    Route_Examples -> PathSegment "examples" $ unitEncoder mempty
    Route_Documentation -> PathSegment "documentation" $ unitEncoder mempty
    Route_Talks -> PathSegment "talks" $ maybeEncoder (unitEncoder mempty) $ pathComponentEncoder $ \case
      Talk_PracticalFRP -> PathSegment "practical-frp" $ pathComponentEncoder $ \case
        PracticalFRP_Part1 -> PathSegment "part-1" $ unitEncoder mempty
        PracticalFRP_Part2 -> PathSegment "part-2" $ unitEncoder mempty
      Talk_RealWorld -> PathSegment "real-world" $ unitEncoder mempty
      Talk_BrowserProgramming -> PathSegment "browser-programming" $ unitEncoder mempty
      Talk_Cochleagram -> PathSegment "cochleagram" $ unitEncoder mempty
      Talk_ReflexDomWithCss -> PathSegment "reflex-dom-with-css" $ unitEncoder mempty
    Route_FAQ -> PathSegment "faq" $ unitEncoder mempty

-- | Provide a human-readable name for a given section
sectionTitle :: Some Route -> Text
sectionTitle (Some.This sec) = case sec of
  Route_Home -> "Home"
  Route_Tutorials -> "Tutorials"
  Route_Examples -> "Examples"
  Route_Documentation -> "Documentation"
  Route_Talks -> "Talks"
  Route_FAQ -> "FAQ"

-- | Provide a human-readable name for a route
routeTitle :: R Route -> Text
routeTitle (sec :=> _) = sectionTitle $ Some.This sec

-- | Provide a human-readable description for a given section
sectionDescription :: Some Route -> Text
sectionDescription (Some.This sec) = case sec of
  Route_Home -> "Reflex: Practical Functional Reactive Programming"
  Route_Tutorials -> "Tutorials and Guides"
  Route_Examples -> "Examples and Explanations"
  Route_Documentation -> "Documentation and Reference"
  Route_Talks -> "Talks and Presentations"
  Route_FAQ -> "Frequently Asked Questions"

-- | Provide a human-readable description for a given route
routeDescription :: R Route -> Text
routeDescription (sec :=> _) = sectionDescription $ Some.This sec

-- | Given a section, provide its default route
sectionHomepage :: Some Route -> R Route
sectionHomepage (Some.This sec) = sec :/ case sec of
  Route_Home -> ()
  Route_Tutorials -> ()
  Route_Examples -> ()
  Route_Documentation -> ()
  Route_Talks -> Nothing
  Route_FAQ -> ()

-- | Provide a human-readable name for a given talk
talkTitle :: Some Talk -> Text
talkTitle (Some.This talk) = case talk of
  Talk_PracticalFRP -> "Reflex: Practical Functional Reactive Programming (Ryan Trinkle)"
  Talk_RealWorld -> "Real World Reflex (Doug Beardsley)"
  Talk_BrowserProgramming -> "FRP Browser Programming (Niklas Hambüchen)"
  Talk_Cochleagram -> "Reflex Cochleagram (Greg Hale)"
  Talk_ReflexDomWithCss -> "Using Reflex-Dom with CSS"

-- | Given a section, provide its default route
talkHomepage :: Some Talk -> R Talk
talkHomepage (Some.This talk) = talk :/ case talk of
  Talk_PracticalFRP -> PracticalFRP_Part1 :/ ()
  Talk_RealWorld -> ()
  Talk_BrowserProgramming -> ()
  Talk_Cochleagram -> ()
  Talk_ReflexDomWithCss -> ()

-- | The youtube video identifiers for each talk
talkYoutubeId :: R Talk -> Text
talkYoutubeId = \case
  Talk_PracticalFRP :=> Identity pfrp -> case pfrp of
    PracticalFRP_Part1 :=> _ -> "mYvkcskJbc4"
    PracticalFRP_Part2 :=> _ -> "3qfc9XFVo2c"
  Talk_RealWorld :=> _ -> "dNBUDAU9sv4"
  Talk_BrowserProgramming :=> _ -> "dNGClNsnn24"
  Talk_Cochleagram :=> _ -> "MfXxuy_CJSk"
  Talk_ReflexDomWithCss :=> _ -> "QNQaJLNKJQA"
