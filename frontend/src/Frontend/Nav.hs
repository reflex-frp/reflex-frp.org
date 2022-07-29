{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Nav (nav) where

import Common.Route
import Control.Monad (forM_)
import Data.Dependent.Sum (DSum((:=>)))
import Data.Some (Some(..))
import qualified Data.Some as Some
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom

import Frontend.CommonWidgets

-- | Build the entire nav bar
nav
  :: ( DomBuilder t m
     , PostBuild t m
     , Routed t (R Route) m
     , RouteToUrl (R Route) m
     , SetRoute t (R Route) m
     , Prerender js t m
     )
  => m ()
nav = el "nav" $ do
  el "ul" $ el "li" $
    routeLinkScrollToTop (sectionHomepage (Some.Some Route_Home)) $ do
      -- We use CSS to decide which of these logos to show
      divClass "light-theme" reflexLogo
      divClass "dark-theme" reflexLogoInverted
  el "ul" menu

-- | Build the nav's tabs
menu
  :: ( DomBuilder t m
     , PostBuild t m
     , SetRoute t (R Route) m
     , Routed t (R Route) m
     , RouteToUrl (R Route) m
     , Prerender js t m
     )
  => m ()
menu = do
  -- Get the current route, so that we can highlight the corresponding tab
  currentTab <- askRoute
  let currentTabDemux = demux $ fmap (\(sec :=> _) -> Some sec) currentTab
      tabs =
        [ Some Route_GetStarted
        , Some Route_Tutorial
        , Some Route_Resources
        ]
  forM_ tabs $ \section -> do
    -- Create a link that is highlighted if it is the current section
    let thisTabIsSelected = demuxed currentTabDemux section
        highlight = ffor thisTabIsSelected $ \case
          True -> "selected"
          False -> ""
    elDynClass "li" highlight $ routeLinkScrollToTop (sectionHomepage section) $ text $ sectionTitle section
