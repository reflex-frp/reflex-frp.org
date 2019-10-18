{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
module Frontend (frontend) where

import Common.Route
import Frontend.Head
import Frontend.Footer
import Frontend.Nav
import Frontend.Page.Home
import Frontend.Page.GetStarted
import Frontend.Page.Resources

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

frontend :: Frontend (R Route)
frontend = Frontend
  { _frontend_head = pageHead
  , _frontend_body = do
      el "header" nav
      el "main" $ do
        subRoute_ $ \case
          Route_Home -> home
          Route_GetStarted -> sectionPage (Route_GetStarted :/ ()) getStarted
          Route_Resources -> sectionPage (Route_Resources :/ ()) resources
      el "footer" footer
  }
