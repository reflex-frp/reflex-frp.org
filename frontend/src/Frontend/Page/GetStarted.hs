{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Frontend.Page.GetStarted (getStarted) where

import Control.Monad (forM_)
import Control.Monad.Fix
import Data.Dependent.Sum (DSum(..))
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Text (Text)
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Reflex.Dom
import Frontend.FontAwesome
import Frontend.CommonWidgets

import Common.Route

getStarted
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , RouteToUrl (R Route) m
     , SetRoute t (R Route) m
     )
  => RoutedT t (Maybe (R Talk)) m ()
getStarted = do
  installation
  tutorials
  t <- talks
  faq
  return t

installation :: DomBuilder t m => m ()
installation = el "div" $ do
  el "h3" $ text "Install"
  el "p" $ do
    text "Please install "
    let obSrc = "https://github.com/obsidiansystems/obelisk"
    extLink obSrc $ text "Obelisk"
    text " to quickly get started with Reflex."

tutorials :: DomBuilder t m => m ()
tutorials = el "div" $ do
  el "h3" $ text "Tutorials"
  el "p" $ do
    el "ol" $ do
      el "li" $ do
        el "label" $ text "QFPL: "
        extLink "https://qfpl.io/projects/reflex/" $
          text "Blog posts and tutorials covering basics of FRP and Reflex"
      el "li" $ do
        el "label" $ text "Beginner Friendly Tutorial: "
        extLink "https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md" $ text "reflex-dom-inbits"

talks
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , RouteToUrl (R Route) m
     , SetRoute t (R Route) m
     )
  => RoutedT t (Maybe (R Talk)) m ()
talks = el "div" $ do
  el "h3" $ routeLink (Route_GetStarted :/ Nothing) $ text "Talks and Presentations"
  divClass "talks" $ do
    let index = forM_ orderedTalks $ elClass "article" "talk" . talkPreview
    maybeRoute_ index $ talk =<< askRoute

-- | Shows a preview image and title for a given Talk
talkPreview
  :: ( DomBuilder t m
     , RouteToUrl (R Route) m
     , SetRoute t (R Route) m
     )
  => Either ExternalTalk (Some Talk)
  -> m ()
talkPreview t = linkToTalk (talkDefaultTarget t) $ el "figure" $ do
  talkPreviewImage t
  el "figcaption" $ do
    text $ talkTitle t
    case (talkDefaultTarget t) of
      (Left _) -> icon_ "external-link-alt"
      _ -> blank

-- | Displays the video for a given Talk
talk
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , RouteToUrl (R Route) m
     , SetRoute t (R Route) m
     )
  => Dynamic t (R Talk)
  -> RoutedT t (R Talk) m ()
talk r = do
    let title (k :=> _) = talkTitle $ Right . Some.This $ k
    el "h4" $ dynText $ fmap title r
    talkEmbed r
    let textLink target = elClass "span" "link" . linkToTalk (Right target) . text
    subRoute_ $ \case
      Talk_PracticalFRP -> subRoute_ $ \case
        PracticalFRP_Part1 ->
          textLink (Talk_PracticalFRP :/ PracticalFRP_Part2 :/ () :: R Talk) "Go to Part 2"
        PracticalFRP_Part2 ->
          textLink (Talk_PracticalFRP :/ PracticalFRP_Part1 :/ () :: R Talk) "Go to Part 1"
      _ -> return ()

-- | Wraps a widget so that it becomes a link to a particular Talk section
linkToTalk
  :: ( DomBuilder t m
     , RouteToUrl (R Route) m
     , SetRoute t (R Route) m
     )
  => Either Text (R Talk)
  -> m ()
  -> m ()
linkToTalk (Right route) w = routeLink (Route_GetStarted :/ Just route) w
linkToTalk (Left url) w = extLink url w

-- | Embed a Talk's youtube video
talkEmbed :: (DomBuilder t m, PostBuild t m) => Dynamic t (R Talk) -> m ()
talkEmbed = youtubeEmbed . fmap talkYoutubeId

-- | Embed an automatically-sized youtube video
-- For CSS, see https://www.h3xed.com/web-development/how-to-make-a-responsive-100-width-youtube-iframe-embed
youtubeEmbed :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m ()
youtubeEmbed videoId = elAttr "div" divAttrs $ elDynAttr "iframe" (iframeAttrs <$> videoId) blank
  where
    divAttrs = "style" =: mconcat
      [ "position: relative;"
      , "width: 100%;"
      , "height: 0;"
      , "padding-bottom: 56.25%"
      ]
    iframeAttrs v = mconcat
      [ "style" =: "position:absolute;top:0;left:0;width:100%;height:100%"
      , "src" =: ("https://www.youtube-nocookie.com/embed/" <> v)
      , "frameborder" =: "0"
      , "allow" =: "autoplay; encrypted-media"
      , "allowfullscreen" =: "allowfullscreen"
      ]

-- | The video thumbnail for each talk
-- NB: The executable 'youtubepreviews' in 'backend/src-bin' can be
-- used to retrieve these youtube preview images. Add the talk's youtube
-- identifier to 'talkYoutubeId' and then run 'youtubepreviews'.
talkImage :: Either ExternalTalk (Some Talk) -> Text
talkImage = \case
  Left t -> case t of
    ExternalTalk_GonimoArchitecture -> static @ "img/talk/gonimoTalkThumbnail.jpg"
  Right (Some.This t) -> case t of
    Talk_PracticalFRP -> static @ "img/talk/mYvkcskJbc4.jpg"
    Talk_RealWorld -> static @ "img/talk/dNBUDAU9sv4.jpg"
    Talk_BrowserProgramming -> static @ "img/talk/dNGClNsnn24.jpg"
    Talk_Cochleagram -> static @ "img/talk/MfXxuy_CJSk.jpg"

-- | Retrieve the preview image for a talk
talkPreviewImage :: DomBuilder t m => Either ExternalTalk (Some Talk) -> m ()
talkPreviewImage t =
  let attrs = mconcat
        [ "src" =: talkImage t
        , "alt" =: talkTitle t
        ]
  in elAttr "img" attrs blank

faq :: DomBuilder t m => m ()
faq = el "div" $ do
  el "h3" $ text "FAQs"
  el "p" $ text "FAQ questions coming soon! For now, feel free to ask questions within the Reflex-FRP IRC chat provided below. Thank you!"
