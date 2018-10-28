{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Frontend.Page.Talks (talks) where

import Common.Route
import Control.Monad (forM_)
import Control.Monad.Fix
import Data.Dependent.Sum (DSum(..))
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Text (Text)
import Data.Universe (universe)
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Reflex.Dom

talks
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , SetRoute t (R Route) m
     )
  => RoutedT t (Maybe (R Talk)) m ()
talks = do
  let index = forM_ universe $ elClass "article" "talk" . talkPreview
  maybeRoute_ index $ talk =<< askRoute

-- | Shows a preview image and title for a given Talk
talkPreview
  :: ( DomBuilder t m
     , SetRoute t (R Route) m
     )
  => Some Talk
  -> m ()
talkPreview t = linkTo (Just (talkHomepage t)) $ el "figure" $ do
  talkPreviewImage t
  el "figcaption" $ text $ talkTitle t

-- | Displays the video for a given Talk
talk
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , SetRoute t (R Route) m
     )
  => Dynamic t (R Talk)
  -> RoutedT t (R Talk) m ()
talk r = do
    let title (k :=> _) = talkTitle $ Some.This k
    el "h4" $ dynText $ fmap title r
    talkEmbed r
    subRoute_ $ \case
      Talk_PracticalFRP -> subRoute_ $ \case
        PracticalFRP_Part1 ->
          linkToButton (Just (Talk_PracticalFRP :/ PracticalFRP_Part2 :/ ())) "Go to Part 2"
        PracticalFRP_Part2 ->
          linkToButton (Just (Talk_PracticalFRP :/ PracticalFRP_Part1 :/ ())) "Go to Part 1"
      _ -> return ()

-- | Wraps a widget so that it becomes a link to a particular Talk section
linkTo
  :: ( DomBuilder t m
     , SetRoute t (R Route) m
     )
  => Maybe (R Talk)
  -> m ()
  -> m ()
linkTo route w = do
  let baseRoute = Route_Talks
  (l, _) <- el' "a"  w
  setRoute $ (baseRoute :/ route) <$ domEvent Click l

-- | A button linking to a particular Talk section
linkToButton
  :: ( DomBuilder t m
     , SetRoute t (R Route) m
     )
  => Maybe (R Talk)
  -> Text
  -> m ()
linkToButton r = linkTo r . text

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
talkImage :: Some Talk -> Text
talkImage (Some.This t) = case t of
  Talk_PracticalFRP -> static @ "img/talk/mYvkcskJbc4.jpg"
  Talk_RealWorld -> static @ "img/talk/dNBUDAU9sv4.jpg"
  Talk_BrowserProgramming -> static @ "img/talk/dNGClNsnn24.jpg"
  Talk_Cochleagram -> static @ "img/talk/MfXxuy_CJSk.jpg"
  Talk_ReflexDomWithCss -> static @ "img/talk/QNQaJLNKJQA.jpg"

-- | Retrieve the preview image for a talk
talkPreviewImage :: DomBuilder t m => Some Talk -> m ()
talkPreviewImage t =
  let attrs = mconcat
        [ "src" =: talkImage t
        , "alt" =: talkTitle t
        ]
  in elAttr "img" attrs blank
