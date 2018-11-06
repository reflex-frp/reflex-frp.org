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
  Route_Talks :: Route (Maybe (R EmbeddedTalk))
  Route_FAQ :: Route ()
deriving instance Show (Route a)

data EmbeddedTalk :: * -> * where
  EmbeddedTalk_PracticalFRP :: EmbeddedTalk (R PracticalFRP)
  EmbeddedTalk_RealWorld :: EmbeddedTalk ()
  EmbeddedTalk_BrowserProgramming :: EmbeddedTalk ()
  EmbeddedTalk_Cochleagram :: EmbeddedTalk ()
  EmbeddedTalk_ReflexDomWithCss :: EmbeddedTalk ()
deriving instance Show (EmbeddedTalk a)

data Talk =
  Talk_PracticalFRP
  | Talk_RealWorld
  | Talk_GonimoArchitecture
  | Talk_BrowserProgramming
  | Talk_Cochleagram
  | Talk_ReflexDomWithCss
  deriving (Enum, Bounded)

data PracticalFRP :: * -> * where
  PracticalFRP_Part1 :: PracticalFRP ()
  PracticalFRP_Part2 :: PracticalFRP ()
deriving instance Show (PracticalFRP a)

deriveRouteComponent ''EmbeddedTalk
deriveRouteComponent ''Route
deriveRouteComponent ''PracticalFRP

-- | Link to external videos
--   since these cannot be embedded, it is not part of the Route
data ExternalLink =
  ExtLink_GonimoArchitecture
  deriving (Show)

backendRouteEncoder :: (check ~ Either Text) => Encoder check Identity (R (Sum Void1 (ObeliskRoute Route))) PageName
backendRouteEncoder = handleEncoder (\_ -> InR (ObeliskRoute_App Route_Home) :/ ()) $ pathComponentEncoder $ \case
  InL v -> case v of {}
  InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
    Route_Home -> PathEnd $ unitEncoder mempty
    Route_Tutorials -> PathSegment "tutorials" $ unitEncoder mempty
    Route_Examples -> PathSegment "examples" $ unitEncoder mempty
    Route_Documentation -> PathSegment "documentation" $ unitEncoder mempty
    Route_Talks -> PathSegment "talks" $ maybeEncoder (unitEncoder mempty) $ pathComponentEncoder $ \case
      EmbeddedTalk_PracticalFRP -> PathSegment "practical-frp" $ pathComponentEncoder $ \case
        PracticalFRP_Part1 -> PathSegment "part-1" $ unitEncoder mempty
        PracticalFRP_Part2 -> PathSegment "part-2" $ unitEncoder mempty
      EmbeddedTalk_RealWorld -> PathSegment "real-world" $ unitEncoder mempty
      EmbeddedTalk_BrowserProgramming -> PathSegment "browser-programming" $ unitEncoder mempty
      EmbeddedTalk_Cochleagram -> PathSegment "cochleagram" $ unitEncoder mempty
      EmbeddedTalk_ReflexDomWithCss -> PathSegment "reflex-dom-with-css" $ unitEncoder mempty
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
talkTitle :: Talk -> Text
talkTitle = \case
  Talk_PracticalFRP -> "Reflex: Practical Functional Reactive Programming (Ryan Trinkle)"
  Talk_RealWorld -> "Real World Reflex (Doug Beardsley)"
  Talk_GonimoArchitecture -> "The Gonimo Architecture"
  Talk_BrowserProgramming -> "FRP Browser Programming (Niklas Hambüchen)"
  Talk_Cochleagram -> "Reflex Cochleagram (Greg Hale)"
  Talk_ReflexDomWithCss -> "Using Reflex-Dom with CSS (Kat Chuang)"

-- | Given a talk, provide its default route or url
talkHomepage :: Talk -> Either Text (R EmbeddedTalk)
talkHomepage = \case
  Talk_PracticalFRP -> Right $ EmbeddedTalk_PracticalFRP :/ PracticalFRP_Part1 :/ ()
  Talk_RealWorld -> Right $ EmbeddedTalk_RealWorld :/ ()
  Talk_GonimoArchitecture ->
    Left "https://skillsmatter.com/skillscasts/12637-the-gonimo-architecture"
  Talk_BrowserProgramming -> Right $ EmbeddedTalk_BrowserProgramming :/ ()
  Talk_Cochleagram -> Right $ EmbeddedTalk_Cochleagram :/ ()
  Talk_ReflexDomWithCss -> Right $ EmbeddedTalk_ReflexDomWithCss :/ ()

-- | The youtube video identifiers for each embedded talk
talkYoutubeId :: R EmbeddedTalk -> Text
talkYoutubeId = \case
  EmbeddedTalk_PracticalFRP :=> Identity pfrp -> case pfrp of
    PracticalFRP_Part1 :=> _ -> "mYvkcskJbc4"
    PracticalFRP_Part2 :=> _ -> "3qfc9XFVo2c"
  EmbeddedTalk_RealWorld :=> _ -> "dNBUDAU9sv4"
  EmbeddedTalk_BrowserProgramming :=> _ -> "dNGClNsnn24"
  EmbeddedTalk_Cochleagram :=> _ -> "MfXxuy_CJSk"
  EmbeddedTalk_ReflexDomWithCss :=> _ -> "QNQaJLNKJQA"

talkForEmbeddedTalk :: R EmbeddedTalk -> Talk
talkForEmbeddedTalk (t :=> _) = case t of
  EmbeddedTalk_PracticalFRP -> Talk_PracticalFRP
  EmbeddedTalk_RealWorld -> Talk_RealWorld
  EmbeddedTalk_BrowserProgramming -> Talk_BrowserProgramming
  EmbeddedTalk_Cochleagram -> Talk_Cochleagram
  EmbeddedTalk_ReflexDomWithCss -> Talk_ReflexDomWithCss

-- | Returns a tuple containing thumbnail download url and file name to save in static/img/talk
talkThumbnail :: Talk -> (Text, Text)
talkThumbnail t = case t of
  Talk_GonimoArchitecture ->
    ("http://i.vimeocdn.com/video/731745473_640.jpg", "gonimoTalkThumbnail")
  _ -> case (talkHomepage t) of
      (Left _) -> error "talkThumbnail: Add thumbnail URL for non-youtube videos"
      (Right r) -> (url, yId)
        where
          yId = talkYoutubeId r
          url = "http://i3.ytimg.com/vi/" <> yId <> "/hqdefault.jpg"
