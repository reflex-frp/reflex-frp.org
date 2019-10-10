{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Frontend.Page.GetStarted (sectionPage, getStarted) where

import Control.Lens ((%~))
import Control.Monad (forM_)
import Control.Monad.Fix
import Data.Dependent.Sum (DSum(..))
import Data.Proxy
import Data.Foldable (for_, traverse_)
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Text (Text)
import qualified Data.Text as T
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Reflex.Dom
import Frontend.FontAwesome
import Frontend.CommonWidgets

import Common.Route

tableOfContents :: (DomBuilder t m, RouteToUrl (R Route) m) => Section m -> m ()
tableOfContents topSection = el "ul" $ do
  el "li" $ titleLink $ _section_title topSection
  go $ _section_subsections topSection
  where
    titleLink t = routeFragment (Route_GetStarted :/ ()) (titleFragment t) (text t)
    go sections = for_ sections $ \section -> el "li" $ do
      titleLink $ _section_title section
      el "ul" $ go $ _section_subsections section

routeFragment
  :: (DomBuilder t m, RouteToUrl (R route) m)
  => R route -> Text -> m a -> m a
routeFragment r frag w = do
  enc <- askRouteToUrl
  elAttr "a" ("href" =: (enc r <> "#" <> frag)) w

header :: DomBuilder t m => Int -> Text -> m ()
header i t = elAttr h ("id" =: titleFragment t) $ text t
  where h = "h" <> T.pack (show $ min 6 $ max 1 i)

goSections :: DomBuilder t m => Int -> [Section m] -> m ()
goSections prec sections = for_ sections $ \section -> do
  header prec $ _section_title section
  _section_content section
  goSections (prec + 1) $ _section_subsections section

titleFragment :: Text -> Text
titleFragment = T.toLower . T.intercalate "-" . T.words

data Section m = Section
  { _section_title :: Text
  , _section_content :: m ()
  , _section_subsections :: [Section m]
  }

sectionPage
  :: (DomBuilder t m, RouteToUrl (R Route) m)
  => Section m -> m ()
sectionPage section = do
  header 1 $ _section_title section
  _section_content section
  el "hr" blank
  el "nav" $ tableOfContents section
  el "article" $ goSections 2 $ _section_subsections section

getStarted :: DomBuilder t m => Section m
getStarted = Section
  { _section_title = "Getting Started"
  , _section_content = do
    el "p" $ text "This page provides links to information on Reflex documentation and related resources."
    el "p" $ text "Reflex-FRP is a Haskell-based ecosystem for building user interfaces and web apps. Learn what makes Reflex unique on our home page."
  , _section_subsections = [tryReflexFRP, learnReflexFRP]
  }

tryReflexFRP :: DomBuilder t m => Section m
tryReflexFRP = Section
  { _section_title = "Try Reflex-FRP"
  , _section_content = el "p" $ text "The Reflex ecosystem was created by developers, for developers, to provide a better way to build apps. No matter what you’re working on, it can benefit from Reflex. Here are a few ways to incorporate Reflex-FRP into your project:"
  , _section_subsections = [createANewApp, buildAFrontend, convertExistingApp, testDrive]
  }

createANewApp :: DomBuilder t m => Section m
createANewApp = Section
  { _section_title = "Create a New Reflex App"
  , _section_content = do
    el "p" $ text "Get started with Obelisk to build a full-stack web and mobile application powered by Reflex. Obelisk will set you up with access to Reflex libraries, developer tools, and everything you need to begin work on your client and server."
    el "p" $ text "Obelisk has built-in support for deployments, server-side rendering, routing, and smooth developer workflow. It’s the best way to both develop and deploy your Reflex project for web or mobile, and getting started is easy."
  , _section_subsections = []
  }

buildAFrontend :: DomBuilder t m => Section m
buildAFrontend = Section
  { _section_title = "Build a Frontend in Reflex"
  , _section_content = do
    el "p" $ text "Get started with Reflex Platform, the most flexible way to build a Reflex frontend for your application."
    el "p" $ text "Build user interfaces for applications on desktop, mobile, terminal, or the web with a curated set of packages and tools that have been tested across all platforms."
    el "p" $ text "With support for multiple platforms, management of cross-platform dependencies, and interoperability with existing apps, Reflex Platform is the best choice for writing a new front end or finishing off a back end."
    el "p" $ text "Get started quickly with Reflex Platform here."
  , _section_subsections = []
  }

convertExistingApp :: DomBuilder t m => Section m
convertExistingApp = Section
  { _section_title = "Convert Parts of an Existing App to Reflex"
  , _section_content = do
    el "p" $ text "Start using Reflex like you would any other Haskell library to convert portions of an existing application."
    el "p" $ text "Download the Reflex library from Hackage and begin building interactive components in pure functional style with Cabal or Stack. This allows you to incrementally introduce FRP concepts into your codebase, and create Reflex widgets that integrate with existing Haskell code."
    el "p" $ text "You can also download and use the Reflex VTY and Reflex DOM packages independently. Enjoy complete flexibility coupled with the full power of FRP. Integrate Reflex into your applications however you see fit."
  , _section_subsections = []
  }

testDrive :: DomBuilder t m => Section m
testDrive = Section
  { _section_title = "Take FRP for a Test Drive"
  , _section_content = do
    el "p" $ text "Use Reflex Platform to run Try Reflex if you're looking to get a casual and quick start, but not commit to an entire project."
    el "p" $ text "This is an easy way to become familiar with FRP concepts without worrying about servers or an application. Fast, simple, get started here with Reflex Platform."
  , _section_subsections = []
  }

learnReflexFRP :: DomBuilder t m => Section m
learnReflexFRP = Section
  { _section_title = "Learn Reflex-FRP"
  , _section_content = el "p" $ text "A functional understanding of Haskell is all that’s needed to begin learning Reflex-FRP. If you’d like more information on coding in Haskell, scroll down to ‘Haskell for Beginners’."
  , _section_subsections = [reflexBasics, reflexExamples, reflexExtended, haskellForBeginners, theTutorial]
  }

reflexBasics :: DomBuilder t m => Section m
reflexBasics = Section
  { _section_title = "Reflex-FRP Basics"
  , _section_content = do
    el "p" $ text "If FRP is a new paradigm for you, give yourself the time to learn its declarative style. Below are some great resources on the basics of Reflex. Use these to kickstart your learning, or just familiarise yourself with Reflex terminology before beginning our tutorial."
    el "ul" $ do
      el "li" $ text "Read through this written Reflex quick guide."
      el "li" $ text "Watch this video on the basics of FRP."
  , _section_subsections = []
  }

reflexExamples :: DomBuilder t m => Section m
reflexExamples = Section
  { _section_title = "Reflex Examples"
  , _section_content = do
    el "p" $ text "There is an abundance of resources available as you become more familiar with FRP, beginning with some basic examples of Reflex in use."
    el "p" $ text "You can toy with a basic to-do list, a file reader, and an on-screen keyboard, as well as many other examples, all made with Reflex using Obelisk."
    el "p" $ text "Get tinkering with any of these examples in less than 5 minutes by downloading Obelisk. Run them locally to get a feel for Reflex, or even use them as a starting point for building your own application."
  , _section_subsections = []
  }

reflexExtended :: DomBuilder t m => Section m
reflexExtended = Section
  { _section_title = "Reflex Extended Education"
  , _section_content = do
    el "p" $ text "Check out this educational introduction to Reflex. The project consists of two talks and a 13-part guide on the basics of Reflex and the use cases where it makes the most significant impact."
    el "p" $ text "If you’re more interested in writing a GUI in Haskell, read this beginner-friendly step by step tutorial for Reflex DOM. It explains in detail how to write GUI programs that run in a web browser or as a desktop application, in a functional way."
    el "p" $ text "If you’d prefer a more advanced take on using Reflex DOM to build front-end web applications, check out this fantastic one. It has a higher-level feel, intending to get you up to speed on building apps quickly."
  , _section_subsections = []
  }

haskellForBeginners :: DomBuilder t m => Section m
haskellForBeginners = Section
  { _section_title = "Haskell for Beginners"
  , _section_content = do
    el "p" $ text "If you’re not yet familiar with Haskell, this series of online lessons from the University of Pennsylvania is approachable, is informative, and walks you through the basics of the language. With reading recommendations and ‘homework’ problems, this is an academic approach to learning Haskell."
    el "p" $ text "Similarly, this 12-part series is a gentle introduction to Haskell aimed at those with an imperative background. No previous experience with functional programming or advanced mathematics is required. Sort through the well-named chapters if you’d like to brush up on a specific element of Haskell, or read all the way through for a comprehensive education."
  , _section_subsections = []
  }

theTutorial :: DomBuilder t m => Section m
theTutorial = Section
  { _section_title = "The Tutorial"
  , _section_content = do
    el "p" $ text "If you feel comfortable with your Haskell abilities and have done all the ‘reading up’ on FRP you care to, get started on our Reflex tutorial! The tutorial walks you through building a simple functional reactive calculator that can be used in a web browser, beginning with the download instructions for Nix and Obelisk."
    el "p" $ text "Even if you don’t plan to build a calculator for your own project, this tutorial is helpful as it teaches the fundamentals of building a Reflex application, and the key concepts at play. A familiarity with this process will make developing other applications much easier. If you get stuck, try referencing our FAQ section, resources page, or Reflex live-chat."
  , _section_subsections = []
  }

