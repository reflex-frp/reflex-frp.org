{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Frontend.Footer (footer) where

import Obelisk.Frontend.GoogleAnalytics
import Obelisk.Generated.Static
import Reflex.Dom

import Frontend.CommonWidgets

footer :: (Analytics t m, DomBuilder t m) => m ()
footer = do
  el "nav" $ elClass "ul" "social" $ do
    el "li" $ extLinkAttr ("title" =: "Twitter") "https://twitter.com/reflexfrp" $ do
      elClass "i" "icon-twitter" blank
      text "@reflexfrp"
    el "li" $ extLinkAttr ("title" =: "Reddit") "https://reddit.com/r/reflexfrp" $ do
      elClass "i" "fa-solid fa-brands fa-reddit" blank
      text "r/reflexfrp"
    el "li" $ extLinkAttr ("title" =: "irc") "https://web.libera.chat/#reflex-frp" $ do
      el "i" $ elAttr "img" ("src" =: static @"img/libera.svg") blank
      text "#reflex-frp"
