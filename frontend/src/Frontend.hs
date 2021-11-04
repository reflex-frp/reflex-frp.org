{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend (frontend) where

import Common.Route
import Frontend.CommonWidgets
import Frontend.Head
import Frontend.Footer
import Frontend.Nav
import Frontend.Page.Home
import Frontend.Page.GetStarted
import Frontend.Page.Resources
import Frontend.Page.Tutorial

import qualified Data.Map as Map
import Data.Text (Text)
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Frontend.GoogleAnalytics
import Reflex.Dom.Core

frontend :: Frontend (R Route)
frontend = Frontend
  { _frontend_head = pageHead
  , _frontend_body = mapRoutedT runGoogleAnalyticsT $ minWidth "1140px" $ do
      el "header" nav
      subRoute_ $ \case
        Route_Home -> home
        Route_GetStarted -> sectionPage (Route_GetStarted :/ ()) getStarted
        Route_Tutorial -> do
          el "main" $ el "article" tutorial
          -- Prism is in prerender so that it doesn't muck with the DOM until hydration is finished.
          -- It's here rather than in the head such that it runs when switching to this page with JS.
          prerender_ blank $ elAttr "script" ("type" =: "text/javascript" <> "src" =: static @"js/prism.js") blank
        Route_Resources -> sectionPage (Route_Resources :/ ()) resources
      el "footer" footer
  }

minWidth :: DomBuilder t m => Text -> m a -> m a
minWidth txt = elAttr "div" (Map.singleton "style" $ "min-width: " <> txt)
