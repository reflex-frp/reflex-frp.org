{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Frontend.Page.GetStarted (getStarted) where

import Obelisk.Route.Frontend
import Obelisk.Frontend.GoogleAnalytics (Analytics)
import Reflex.Dom
import Frontend.CommonWidgets

import Common.Route

getStarted :: (Analytics t m, DomBuilder t m, RouteToUrl (R Route) m, SetRoute t (R Route) m, Prerender js t m) => Section m
getStarted = Section
  { _section_title = "Getting Started"
  , _section_content = do
    elClass "p" "description" $ text "This page provides links to information on Reflex documentation and related resources."
    elClass "p" "description" $ do
      text "Reflex-FRP is a Haskell-based ecosystem for building user interfaces and web apps. Learn what makes Reflex unique on our "
      routeLinkScrollToTop (Route_Home :/ ()) $ text "home page"
      text "."
  , _section_subsections = [tryReflexFRP, learnReflexFRP]
  }

tryReflexFRP :: (Analytics t m, DomBuilder t m) => Section m
tryReflexFRP = Section
  { _section_title = "Try Reflex-FRP"
  , _section_content = elClass "p" "description" $ text "The Reflex ecosystem was created by developers, for developers, to provide a better way to build apps. No matter what you’re working on, it can benefit from Reflex. Here are a few ways to incorporate Reflex-FRP into your project:"
  , _section_subsections = [createANewApp, buildAFrontend, convertExistingApp, testDrive]
  }

createANewApp :: (Analytics t m, DomBuilder t m) => Section m
createANewApp = Section
  { _section_title = "Create a New Reflex App"
  , _section_content = do
    el "p" $ do
      text "Get started with "
      extLink "https://github.com/obsidiansystems/obelisk" $ text "Obelisk"
      text " to build a full-stack web and mobile application powered by Reflex. Obelisk will set you up with access to Reflex libraries, developer tools, and everything you need to begin work on your client and server."
    el "p" $ do
      text "Obelisk has built-in support for deployments, server-side rendering, routing, and smooth developer workflow. It’s the best way to both develop and deploy your Reflex project for web or mobile, and "
      extLink "https://github.com/obsidiansystems/obelisk#installing-obelisk" $ text "getting started"
      text " is easy."
  , _section_subsections = []
  }

buildAFrontend :: (Analytics t m, DomBuilder t m) => Section m
buildAFrontend = Section
  { _section_title = "Build a Frontend in Reflex"
  , _section_content = do
    el "p" $ do
      text "Get started with "
      extLink "https://github.com/reflex-frp/reflex-platform" $ text "Reflex Platform"
      text ", the most flexible way to build a Reflex frontend for your application."
    el "p" $ text "Build user interfaces for applications on desktop, mobile, terminal, or the web with a curated set of packages and tools that have been tested across all platforms."
    el "p" $ text "With support for multiple platforms, management of cross-platform dependencies, and interoperability with existing apps, Reflex Platform is the best choice for writing a new front end or finishing off a back end."
    el "p" $ do
      text "Get started quickly with Reflex Platform "
      extLink "https://github.com/reflex-frp/reflex-platform" $ text "here"
      text "."
  , _section_subsections = []
  }

convertExistingApp :: (Analytics t m, DomBuilder t m) => Section m
convertExistingApp = Section
  { _section_title = "Convert Parts of an Existing App to Reflex"
  , _section_content = do
    el "p" $ text "Start using Reflex like you would any other Haskell library to convert portions of an existing application."
    el "p" $ do
      text "Download the Reflex library from "
      extLink "http://hackage.haskell.org/package/reflex" $ text "Hackage"
      text " and begin building interactive components in pure functional style with Cabal or Stack. This allows you to incrementally introduce FRP concepts into your codebase, and create Reflex widgets that integrate with existing Haskell code."
    el "p" $ text "You can also download and use the Reflex VTY and Reflex DOM packages independently. Enjoy complete flexibility coupled with the full power of FRP. Integrate Reflex into your applications however you see fit."
  , _section_subsections = []
  }

testDrive :: (Analytics t m, DomBuilder t m) => Section m
testDrive = Section
  { _section_title = "Take FRP for a Test Drive"
  , _section_content = do
    el "p" $ do
      text "Use Reflex Platform to run "
      extLink "https://github.com/reflex-frp/reflex-platform/blob/develop/try-reflex" $ text "Try Reflex"
      text " if you're looking to get a casual and quick start, but not commit to an entire project."
    el "p" $ do
      text "This is an easy way to become familiar with FRP concepts without worrying about servers or an application. Fast, simple, get started "
      extLink "https://github.com/reflex-frp/reflex-platform" $ text "here"
      text " with Reflex Platform."
  , _section_subsections = []
  }

learnReflexFRP :: forall js t m. (Analytics t m, DomBuilder t m, SetRoute t (R Route) m, RouteToUrl (R Route) m, Prerender js t m) => Section m
learnReflexFRP = Section
  { _section_title = "Learn Reflex-FRP"
  , _section_content = elClass "p" "description" $ do
    text "A functional understanding of Haskell is all that’s needed to begin learning Reflex-FRP. If you’d like more information on coding in Haskell, scroll down to "
    routeFragment (Route_GetStarted :/ ())
      (titleFragment $ _section_title (haskellForBeginners :: Section m))
      (text "‘Haskell for Beginners’")
    text "."
  , _section_subsections = [reflexBasics, reflexExtended, haskellForBeginners, theTutorial]
  }

reflexBasics :: (Analytics t m, DomBuilder t m) => Section m
reflexBasics = Section
  { _section_title = "Reflex-FRP Basics"
  , _section_content = do
    el "p" $ text "If FRP is a new paradigm for you, give yourself the time to learn its declarative style. Below are some great resources on the basics of Reflex. Use these to kickstart your learning, or just familiarise yourself with Reflex terminology before beginning our tutorial."
    el "ul" $ do
      el "li" $ el "p" $ do
        text "Read through this written "
        extLink "http://docs.reflex-frp.org/en/latest/reflex_docs.html" $ text "Reflex quick guide"
        text "."
      el "li" $ el "p" $ do
        text "Watch this video on "
        extLink "https://www.youtube.com/watch?v=mYvkcskJbc4" $ text "the basics of FRP"
        text "."
  , _section_subsections = []
  }

reflexExtended :: (Analytics t m, DomBuilder t m) => Section m
reflexExtended = Section
  { _section_title = "Reflex Extended Education"
  , _section_content = do
    el "p" $ do
      text "Check out this educational "
      extLink "https://qfpl.io/projects/reflex/" $ text "introduction to Reflex"
      text ". The project consists of two talks and a 13-part guide on the basics of Reflex and the use cases where it makes the most significant impact."
    el "p" $ do
      text "If you’re more interested in writing a GUI in Haskell, read this beginner-friendly "
      extLink "https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md" $ text "step by step tutorial"
      text " for Reflex DOM. It explains in detail how to write GUI programs that run in a web browser or as a desktop application, in a functional way."
    el "p" $ do
      text "If you’d prefer a more advanced take on using Reflex DOM to build front-end web applications, check out this "
      extLink "https://yowconference.com/talks/ben-kolera/yow-lambda-jam-2019/the-reflex-architecture-combo-workshop-10032/" $ text "fantastic one"
      text ". It has a higher-level feel, intending to get you up to speed on building apps quickly."
  , _section_subsections = []
  }

haskellForBeginners :: (Analytics t m, DomBuilder t m) => Section m
haskellForBeginners = Section
  { _section_title = "Haskell for Beginners"
  , _section_content = do
    el "p" $ do
      text "If you’re not yet familiar with Haskell, this series of "
      extLink "https://www.seas.upenn.edu/~cis194/spring13/lectures.html" $ text "online lessons"
      text " from the University of Pennsylvania is approachable, is informative, and walks you through the basics of the language. With reading recommendations and ‘homework’ problems, this is an academic approach to learning Haskell."
    el "p" $ do
      text "Similarly, this "
      extLink "https://www.schoolofhaskell.com/user/bartosz/basics-of-haskell" $ text "12-part series"
      text " is a gentle introduction to Haskell aimed at those with an imperative background. No previous experience with functional programming or advanced mathematics is required. Sort through the well-named chapters if you’d like to brush up on a specific element of Haskell, or read all the way through for a comprehensive education."
  , _section_subsections = []
  }

theTutorial :: (Analytics t m, DomBuilder t m, SetRoute t (R Route) m, RouteToUrl (R Route) m, Prerender js t m) => Section m
theTutorial = Section
  { _section_title = "The Tutorial"
  , _section_content = do
    el "p" $ text "If you feel comfortable with your Haskell abilities and have done all the ‘reading up’ on FRP you care to, get started on our Reflex tutorial! The tutorial walks you through building a simple functional reactive calculator that can be used in a web browser, beginning with the download instructions for Nix and Obelisk."
    el "p" $ do
      text "Even if you don’t plan to build a calculator for your own project, this tutorial is helpful as it teaches the fundamentals of building a Reflex application, and the key concepts at play. A familiarity with this process will make developing other applications much easier. If you get stuck, try referencing our "
      routeLinkScrollToTop (Route_Resources :/ ()) $ text "resources page"
      text ", or "
      extLink "http://webchat.freenode.net?channels=%23reflex-frp&uio=d4" $ text "#reflex-frp on IRC"
      text "."
  , _section_subsections = []
  }

