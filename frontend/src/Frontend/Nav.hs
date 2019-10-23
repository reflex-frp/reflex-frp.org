{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Nav (nav) where

import Common.Route
import Control.Monad (forM_)
import Data.Dependent.Sum (DSum ((:=>)))
import qualified Data.Some as Some
import Data.Universe (universe)
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
nav = do
  reflexLogo
  el "nav" menu

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
  let currentTabDemux = demux $ fmap (\(sec :=> _) -> Some.Some sec) currentTab
  -- Iterate over all the top-level routes except Home
  -- Home is reached by clicking logo
  forM_ universe $ \section -> do
    -- Create a link that is highlighted if it is the current section
    let thisTabIsSelected = demuxed currentTabDemux section
        highlight = ffor thisTabIsSelected $ \case
          True -> "selected"
          False -> ""
    elDynClass "span" highlight $ routeLinkScrollToTop (sectionHomepage section) $ text $ sectionTitle section
