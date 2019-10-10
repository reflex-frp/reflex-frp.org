{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
module Frontend (frontend) where

import Common.Route
import Data.Dependent.Sum (DSum(..))
import Frontend.Head
import Frontend.Nav
import Frontend.Page.Home
import Frontend.Page.GetStarted

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
          Route_GetStarted -> sectionPage getStarted
      el "footer" footer
  }

footer :: (RouteToUrl (R Route) m, SetRoute t (R Route) m, DomBuilder t m) => m ()
footer = do
  text "REFLEX" -- TODO logo
  elClass "section" "social" $ do
    externalLinkWithTitle "Twitter" "https://twitter.com" $ elClass "i" "icon-twitter" blank -- TODO
    externalLinkWithTitle "Medium" "https://medium.com" $ elClass "i" "icon-medium" blank -- TODO
  elClass "section" "links" $ do
    let category title content = el "article" $ do
          el "h5" $ text title
          content
    category "Ecosystem" $ do
      routeLink (Route_Home :/ ()) $ text "Home"
      routeLink (Route_GetStarted :/ ()) $ text "Get Started"
      routeLink (Route_Resources :/ ()) $ text "Docs" -- TODO is this correct?
    category "Community" $ do
      externalLink "https://reflex-frp.org" $ text "Reflex Blog" -- TODO
      externalLink "https://reddit.com/r/reflexfrp" $ text "Reddit"
      externalLink "http://webchat.freenode.net?channels=%23reflex-frp&uio=d4" $ text "IRC"
      externalLink "https://reflex-frp.org" $ text "Events" -- TODO
      externalLink "https://reflex-frp.org" $ text "Use Cases" -- TODO
  where
    externalLink url = elAttr "a" ("href" =: url)
    externalLinkWithTitle title url = elAttr "a" ("href" =: url <> "title" =: title)
