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
  Route_GetStarted :: Route (Maybe (R Talk))
  Route_Documentation :: Route ()
  Route_Examples :: Route ()
deriving instance Show (Route a)

data Talk :: * -> * where
  Talk_PracticalFRP :: Talk (R PracticalFRP)
  Talk_RealWorld :: Talk ()
  Talk_BrowserProgramming :: Talk ()
  Talk_Cochleagram :: Talk ()
deriving instance Show (Talk a)

data PracticalFRP :: * -> * where
  PracticalFRP_Part1 :: PracticalFRP ()
  PracticalFRP_Part2 :: PracticalFRP ()
deriving instance Show (PracticalFRP a)

data ExternalTalk =
  ExternalTalk_GonimoArchitecture
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Universe ExternalTalk

deriveRouteComponent ''Route
deriveRouteComponent ''Talk
deriveRouteComponent ''PracticalFRP

backendRouteEncoder :: (check ~ Either Text) => Encoder check Identity (R (Sum Void1 (ObeliskRoute Route))) PageName
backendRouteEncoder = handleEncoder (\_ -> InR (ObeliskRoute_App Route_Home) :/ ()) $ pathComponentEncoder $ \case
  InL v -> case v of {}
  InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
    Route_Home -> PathEnd $ unitEncoder mempty
    Route_Examples -> PathSegment "examples" $ unitEncoder mempty
    Route_Documentation -> PathSegment "docs" $ unitEncoder mempty
    Route_GetStarted -> PathSegment "getstarted" $ maybeEncoder (unitEncoder mempty) $ pathComponentEncoder $ \case
      Talk_PracticalFRP -> PathSegment "practical-frp" $ pathComponentEncoder $ \case
        PracticalFRP_Part1 -> PathSegment "part-1" $ unitEncoder mempty
        PracticalFRP_Part2 -> PathSegment "part-2" $ unitEncoder mempty
      Talk_RealWorld -> PathSegment "real-world" $ unitEncoder mempty
      Talk_BrowserProgramming -> PathSegment "browser-programming" $ unitEncoder mempty
      Talk_Cochleagram -> PathSegment "cochleagram" $ unitEncoder mempty

-- | Provide a human-readable name for a given section
sectionTitle :: Some Route -> Text
sectionTitle (Some.This sec) = case sec of
  Route_Home -> "Home"
  Route_Examples -> "Examples"
  Route_Documentation -> "Docs"
  Route_GetStarted -> "Get Started"

-- | Provide a human-readable name for a route
routeTitle :: R Route -> Text
routeTitle (sec :=> _) = sectionTitle $ Some.This sec

-- | Provide a human-readable description for a given section
sectionDescription :: Some Route -> Text
sectionDescription (Some.This sec) = case sec of
  Route_Home -> "Reflex: Practical Functional Reactive Programming"
  Route_Examples -> "Examples and open source websites built using Reflex"
  Route_Documentation -> "Documentation and Quick Reference"
  Route_GetStarted -> "Installation, Tutorials / Talks and FAQs"

-- | Provide a human-readable description for a given route
routeDescription :: R Route -> Text
routeDescription (sec :=> _) = sectionDescription $ Some.This sec

-- | Given a section, provide its default route
sectionHomepage :: Some Route -> R Route
sectionHomepage (Some.This sec) = sec :/ case sec of
  Route_Home -> ()
  Route_Examples -> ()
  Route_Documentation -> ()
  Route_GetStarted -> Nothing

-- | Provide a human-readable name for a given talk
talkTitle :: Either ExternalTalk (Some Talk) -> Text
talkTitle (Left talk) = case talk of
  ExternalTalk_GonimoArchitecture -> "The Gonimo Architecture"

talkTitle (Right (Some.This talk)) = case talk of
  Talk_PracticalFRP -> "Reflex: Practical Functional Reactive Programming (Ryan Trinkle)"
  Talk_RealWorld -> "Real World Reflex (Doug Beardsley)"
  Talk_BrowserProgramming -> "FRP Browser Programming (Niklas Hambüchen)"
  Talk_Cochleagram -> "Reflex Cochleagram (Greg Hale)"

talkDefaultTarget :: Either ExternalTalk (Some Talk) -> Either Text (R Talk)
talkDefaultTarget = bimap talkExternalUrl talkHomepage

-- | Given a Talk, provide its external url
talkExternalUrl :: ExternalTalk -> Text
talkExternalUrl = \case
  ExternalTalk_GonimoArchitecture ->
    "https://skillsmatter.com/skillscasts/12637-the-gonimo-architecture"

-- | Given a Talk, provide its default route
talkHomepage :: Some Talk -> (R Talk)
talkHomepage (Some.This talk) = talk :/ case talk of
  Talk_PracticalFRP -> PracticalFRP_Part1 :/ ()
  Talk_RealWorld -> ()
  Talk_BrowserProgramming -> ()
  Talk_Cochleagram -> ()

-- | The youtube video identifiers for each talk
talkYoutubeId :: R Talk -> Text
talkYoutubeId = \case
  Talk_PracticalFRP :=> Identity pfrp -> case pfrp of
    PracticalFRP_Part1 :=> _ -> "mYvkcskJbc4"
    PracticalFRP_Part2 :=> _ -> "3qfc9XFVo2c"
  Talk_RealWorld :=> _ -> "dNBUDAU9sv4"
  Talk_BrowserProgramming :=> _ -> "dNGClNsnn24"
  Talk_Cochleagram :=> _ -> "MfXxuy_CJSk"

externalTalkThumbnailUrl :: ExternalTalk -> (Text, Text)
externalTalkThumbnailUrl = \case
  ExternalTalk_GonimoArchitecture ->
    ("gonimoTalkThumbnail", "http://i.vimeocdn.com/video/731745473_640.jpg")

-- | Gives all talks which can be optionally ordered as per need
orderedTalks :: [Either ExternalTalk (Some Talk)]
orderedTalks = nub (firstOnes ++ universe)
  where
    firstOnes = [ f Talk_PracticalFRP
                , f Talk_RealWorld
                , Left ExternalTalk_GonimoArchitecture
                ]
    f = Right . Some.This
