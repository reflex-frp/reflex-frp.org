{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Page.Talks (talks) where

import Common.Route
import Control.Monad (forM_)
import Control.Monad.Fix
import Data.Text (Text)
import Data.Universe (universe)
import Obelisk.Route.Frontend
import Reflex.Dom

talks
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , SetRoute t (R Route) m
     )
  => RoutedT t (Maybe (R Talk)) m ()
talks = do
  elClass "h3" "title" $ do
    text "Talks"
  let linkTo route name = do
        (l, _) <- elAttr' "button" ("type" =: "button") $ text name
        setRoute $ route <$ domEvent Click l
  let index = el "ul" $ forM_ universe $ \talk -> el "li" $ do
        --TODO: It would be nice to have a better way of determining our context
        --(Route_Talks :/) rather than hard-coding it
        linkTo (Route_Talks :/ Just (talkHomepage talk)) $ talkTitle talk
  maybeRoute_ index $ subRoute_ $ \case
    Talk_PracticalFRP -> do
      el "h4" $ text "Reflex: Practical Functional Reactive Programming (Ryan Trinkle)"
      subRoute_ $ \case
        PracticalFRP_Part1 -> do
          youtubeEmbed "mYvkcskJbc4"
          linkTo (Route_Talks :/ Just (Talk_PracticalFRP :/ PracticalFRP_Part2 :/ ())) "Go to Part 2"
        PracticalFRP_Part2 -> do
          youtubeEmbed "3qfc9XFVo2c"
          linkTo (Route_Talks :/ Just (Talk_PracticalFRP :/ PracticalFRP_Part1 :/ ())) "Go to Part 1"
    Talk_RealWorld -> do
      el "h4" $ text "Real World Reflex (Doug Beardsley)"
      youtubeEmbed "dNBUDAU9sv4"
    Talk_BrowserProgramming -> do
      el "h4" $ text "FRP Browser Programming (Niklas Hambüchen)"
      youtubeEmbed "dNGClNsnn24"
    Talk_Cochleagram -> do
      el "h4" $ text "Reflex Cochleagram (Greg Hale)"
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
