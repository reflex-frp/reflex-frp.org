{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Footer (footer) where

import Obelisk.Route.Frontend
import Reflex.Dom

import Common.Route
import Frontend.CommonWidgets

footer :: (RouteToUrl (R Route) m, SetRoute t (R Route) m, DomBuilder t m) => m ()
footer = do
  unfinished "needs logo" $ text "REFLEX"
  elClass "section" "social" $ do
    externalLinkWithTitle "Twitter" "https://twitter.com/reflexfrp" $ elClass "i" "icon-twitter" blank
    externalLinkWithTitle "Medium" "https://medium.com/@obsidian.systems" $ elClass "i" "icon-medium" blank
  elClass "section" "links" $ do
    let category title content = el "article" $ do
          el "h5" $ text title
          content
    category "Ecosystem" $ do
      routeLink (Route_Home :/ ()) $ text "Home"
      routeLink (Route_GetStarted :/ ()) $ text "Get Started"
      routeLink (Route_Resources :/ ()) $ text "Resources"
    category "Community" $ do
      extLink "https://reddit.com/r/reflexfrp" $ text "Reddit"
      extLink "http://webchat.freenode.net?channels=%23reflex-frp&uio=d4" $ text "#reflex-frp on IRC"
      extLink "https://reflex-frp.org" $ unfinished "where is this?" $ text "Use Cases"
  where
    externalLinkWithTitle title url = elAttr "a" ("href" =: url <> "title" =: title)
