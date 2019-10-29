{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
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

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Reflex.Dom.Core

frontend :: Frontend (R Route)
frontend = Frontend
  { _frontend_head = pageHead
  , _frontend_body = do
      el "header" nav
      subRoute_ $ \case
        Route_Home -> home
        Route_GetStarted -> sectionPage (Route_GetStarted :/ ()) getStarted
        Route_Tutorial -> sectionPage (Route_Tutorial :/ ()) tutorial
        Route_Resources -> sectionPage (Route_Resources :/ ()) resources
      el "footer" footer
      -- Prism is in prerender so that it doesn't muck with the DOM until hydration is finished.
      prerender_ blank $ elAttr "script" ("type" =: "text/javascript" <> "src" =: static @"js/prism.js") blank
  }
