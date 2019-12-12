{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Head (pageHead) where

import Data.Map (Map)
import Data.Text
import qualified Data.Text as T
import Obelisk.Configs
import Obelisk.Frontend.GoogleAnalytics
import Obelisk.Generated.Static
import Reflex.Dom
import Obelisk.Route.Frontend

pageHead :: (Routed t r m, DomBuilder t m, Prerender js t m, HasConfigs m) => m ()
pageHead = do
  googleAnalyticsFromConfig
  elAttr "base" ("href" =: "/") blank --TODO: Update obelisk to automatically inject this
  el "title" $ text "Reflex FRP"
  elAttr "meta" metaDesc blank
  elAttr "meta" metaKeywords blank
  elAttr "meta" viewport blank
  pageIcons
  styleSheet $ static @"css/normalize.css"
  styleSheet $ static @"css/fontawesome.min.css"
  styleSheet $ static @"css/font.css"
  styleSheet $ static @"css/style.css"
  styleSheet $ static @"css/icomoon.css"
  -- TODO don't vendor for calculator-tutorial repo
  styleSheet $ static @"calculator/style.css"
  styleSheet $ "https://fonts.googleapis.com/css?family=Poppins&display=swap"
  styleSheet $ "https://fonts.googleapis.com/css?family=DM+Serif+Display&display=swap"
  styleSheet $ "https://fonts.googleapis.com/css?family=Roboto+Mono&display=swap"
  elAttr "script" ("type" =: "text/javascript" <> "src" =: static @"js/processing.min.js") blank
  styleSheet $ static @"css/prism.css"


-- | Link to icons for this page (favicons, etc.)
pageIcons :: DomBuilder t m => m ()
pageIcons = do
  faviconPngLink 16 $ static @"img/icon-16x16.png"
  faviconPngLink 32 $ static @"img/icon-32x32.png"
  faviconPngLink 196 $ static @"img/icon-196x196.png"
  appleTouchIconPngLink 57 $ static @"img/icon-57x57.png"
  appleTouchIconPngLink 60 $ static @"img/icon-60x60.png"
  appleTouchIconPngLink 72 $ static @"img/icon-72x72.png"
  appleTouchIconPngLink 76 $ static @"img/icon-76x76.png"
  appleTouchIconPngLink 114 $ static @"img/icon-114x114.png"
  appleTouchIconPngLink 120 $ static @"img/icon-120x120.png"
  appleTouchIconPngLink 144 $ static @"img/icon-144x144.png"
  appleTouchIconPngLink 152 $ static @"img/icon-152x152.png"

faviconPngLink :: DomBuilder t m => Int -> Text -> m ()
faviconPngLink sz url = elAttr "link" attrs blank
  where attrs = "rel" =: "icon"
             <> "type" =: "image/png"
             <> "size" =: (tshow sz <> "x" <> tshow sz)
             <> "href" =: url

appleTouchIconPngLink :: DomBuilder t m => Int -> Text -> m ()
appleTouchIconPngLink sz url = elAttr "link" attrs blank
  where attrs = "rel" =: "apple-touch-icon"
             <> "sizes" =: (tshow sz <> "x" <> tshow sz)
             <> "href" =: url

metaDesc :: Map Text Text
metaDesc = "name" =: "description"
        <> "content" =: "Reflex Functional Reactive Programming"

metaKeywords :: Map Text Text
metaKeywords = "name" =: "keywords"
            <> "content" =: "reflex, reflex frp, functional reactive programming, haskell, framework, reflex dom"

viewport :: Map Text Text
viewport = "name" =: "viewport"
        <> "content" =: "width=device-width"

--  styleSheet are functions to add links to html <head>
styleSheet :: DomBuilder t m => Text -> m ()
styleSheet myLink = elAttr "link" attrs blank
  where attrs = "rel" =: "stylesheet"
             <> "type" =: "text/css"
             <> "href" =: myLink

tshow :: Show a => a -> Text
tshow = T.pack . show
