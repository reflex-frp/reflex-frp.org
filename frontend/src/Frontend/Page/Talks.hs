{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Frontend.Page.Talks (talks) where

import Control.Monad (forM_)
import Control.Monad.Fix
import Data.Dependent.Sum (DSum(..))
import qualified Data.Some as Some
import Data.Text (Text)
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Reflex.Dom

import Common.Route

talks
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , RouteToUrl (R Route) m
     , SetRoute t (R Route) m
     )
  => RoutedT t (Maybe (R Talk)) m ()
talks = do
  let index = forM_ talkList $ elClass "article" "talk" . talkPreview
      talkList = [ f Talk_PracticalFRP
                 , f Talk_RealWorld
                 , Left ExtLink_GonimoArchitecture
                 , f Talk_BrowserProgramming
                 , f Talk_Cochleagram
                 , f Talk_ReflexDomWithCss
                 ]
      f = Right . Some.This
  maybeRoute_ index $ talk =<< askRoute

-- | Shows a preview image and title for a given Talk
talkPreview
  :: ( DomBuilder t m
     , RouteToUrl (R Route) m
     , SetRoute t (R Route) m
     )
  => EitherEmbeddedOrExtTalk
  -> m ()
talkPreview t = linkToTalk (talkHomepage <$> t) $ el "figure" $ do
  talkPreviewImage t
  el "figcaption" $ text $ talkTitle t

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
    let title (k :=> _) = talkTitle $ (Right . Some.This) k
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
  => Either ExternalLink (R Talk)
  -> m ()
  -> m ()
linkToTalk (Right route) w = routeLink (Route_Talks :/ Just route) w
linkToTalk (Left talk) w = elAttr "a" ("href" =: (externalTalkTarget talk)) w


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
talkImage :: EitherEmbeddedOrExtTalk -> Text
talkImage (Left talk) = case talk of
  ExtLink_GonimoArchitecture -> static @ "img/talk/gonimoTalkThumbnail.jpg"

talkImage (Right (Some.This talk)) = case talk of
  Talk_PracticalFRP -> static @ "img/talk/mYvkcskJbc4.jpg"
  Talk_RealWorld -> static @ "img/talk/dNBUDAU9sv4.jpg"
  Talk_BrowserProgramming -> static @ "img/talk/dNGClNsnn24.jpg"
  Talk_Cochleagram -> static @ "img/talk/MfXxuy_CJSk.jpg"
  Talk_ReflexDomWithCss -> static @ "img/talk/QNQaJLNKJQA.jpg"

-- | Retrieve the preview image for a talk
talkPreviewImage :: DomBuilder t m => EitherEmbeddedOrExtTalk -> m ()
talkPreviewImage t =
  let attrs = mconcat
        [ "src" =: talkImage t
        , "alt" =: talkTitle t
        ]
  in elAttr "img" attrs blank

externalTalkTarget :: ExternalLink -> Text
externalTalkTarget ExtLink_GonimoArchitecture =
  "https://skillsmatter.com/skillscasts/12637-the-gonimo-architecture"
