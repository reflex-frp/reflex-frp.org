{-# LANGUAGE OverloadedStrings #-}
module Frontend.Page.Talks (talks) where

import Data.Text (Text)
import Reflex.Dom

talks :: DomBuilder t m => m ()
talks = do
  elClass "h3" "title" $ do
    text "Talks"
  el "h4" $ text "Reflex: Practical Functional Reactive Programming (Ryan Trinkle)"
  youtubeEmbed "mYvkcskJbc4"
  youtubeEmbed "3qfc9XFVo2c"
  el "h4" $ text "Real World Reflex (Doug Beardsley)"
  youtubeEmbed "dNBUDAU9sv4"
  el "h4" $ text "FRP Browser Programming (Niklas Hambüchen)"
  youtubeEmbed "dNGClNsnn24"
  el "h4" $ text "Reflex (Greg Hale)"
  youtubeEmbed "MfXxuy_CJSk"

-- | Embed an automatically-sized youtube video
-- For CSS, see https://www.h3xed.com/web-development/how-to-make-a-responsive-100-width-youtube-iframe-embed
youtubeEmbed :: DomBuilder t m => Text -> m ()
youtubeEmbed videoId = elAttr "div" ("style" =: "position:relative;width:100%;height:0;padding-bottom:56.25%") $ elAttr "iframe" attrs blank
  where attrs = "style" =: "position:absolute;top:0;left:0;width:100%;height:100%"
             <> "src" =: ("https://www.youtube-nocookie.com/embed/" <> videoId)
             <> "frameborder" =: "0"
             <> "allow" =: "autoplay; encrypted-media"
             <> "allowfullscreen" =: "allowfullscreen"
