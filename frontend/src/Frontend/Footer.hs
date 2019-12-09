{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Footer (footer) where

import Obelisk.Route.Frontend
import Obelisk.Frontend.GoogleAnalytics
import Reflex.Dom

import Common.Route
import Frontend.CommonWidgets

footer :: (Analytics t GtagJSCall m, RouteToUrl (R Route) m, SetRoute t (R Route) m, DomBuilder t m, Prerender js t m) => m ()
footer = do
  reflexLogo
  elClass "section" "social" $ do
    externalLinkWithTitle "Twitter" "https://twitter.com/reflexfrp" $ elClass "i" "icon-twitter" blank
  elClass "section" "links" $ do
    let category title content = el "article" $ do
          el "h5" $ text title
          content
    category "Ecosystem" $ do
      routeLinkScrollToTop (Route_Home :/ ()) $ text "Home"
      routeLinkScrollToTop (Route_GetStarted :/ ()) $ text "Get Started"
      routeLinkScrollToTop (Route_Resources :/ ()) $ text "Resources"
    category "Community" $ do
      extLink "https://reddit.com/r/reflexfrp" $ text "Reddit"
      extLink "http://webchat.freenode.net?channels=%23reflex-frp&uio=d4" $ text "#reflex-frp on IRC"
  where
    externalLinkWithTitle title url = elAttr "a" ("href" =: url <> "title" =: title)
