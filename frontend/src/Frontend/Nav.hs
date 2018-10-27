{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Nav (nav) where

import Common.Route
import Control.Monad (forM_)
import Control.Monad.Fix (MonadFix)
import Data.Dependent.Sum (DSum ((:=>)))
import qualified Data.Some as Some
import Data.Universe (universe)
import Obelisk.Generated.Static
import Obelisk.Route
import Obelisk.Route.Frontend (Routed (askRoute), R, SetRoute (setRoute))
import Reflex.Dom

import Frontend.FontAwesome

-- | Build the entire nav bar, with hamburger menu for expanding on mobile
nav
  :: ( DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     , Routed t (R Route) m
     , SetRoute t (R Route) m
     )
  => Event t () -- ^ When thie event fires, collapse the menu
  -> m ()
nav collapseMenu = do
  openMenu <- divClass "logo-menu" $ do
    -- When the logo is clicked, go to the homepage
    setRoute . fmap (const $ Route_Home :/ ()) =<< logo

    divClass "menu-toggle" $ do
      activeTab <- askRoute
      -- Build the title items, which will only be displayed on small screens
      (currentSection, _) <- elAttr' "a" ("class" =: "current-section") $
        dynText $ routeTitle <$> activeTab
      hamburger <- icon "bars"
      foldDyn ($) False $ leftmost
        [ not <$ domEvent Click currentSection
        , not <$ domEvent Click hamburger
        , const False <$ updated activeTab
        , const False <$ collapseMenu
        ]
  let openAttrs = ffor openMenu $ \case
        True -> "class" =: "active"
        False -> mempty
  elDynAttr "nav" openAttrs menu

-- | Displays the logo and returns an event that fires when the logo is clicked
logo :: DomBuilder t m => m (Event t ())
logo = do
  let logoAttrs = mconcat
        [ "class" =: "logo"
        , "src" =: static @"img/logo.svg"
        , "alt" =: "Reflex"
        ]
  domEvent Click . fst <$> elAttr' "img" logoAttrs blank

-- | Build the nav's tabs
menu
  :: ( DomBuilder t m
     , PostBuild t m
     , SetRoute t (R Route) m
     , Routed t (R Route) m
     )
  => m ()
menu = do
  -- Get the current route, so that we can highlight the corresponding tab
  currentTab <- askRoute
  let currentTabDemux = demux $ fmap (\(sec :=> _) -> Some.This sec) currentTab
  -- Iterate over all the top-level routes
  forM_ universe $ \section -> do
    -- Create a link that is highlighted if it is the current section
    let thisTabIsSelected = demuxed currentTabDemux section
        highlight = ffor thisTabIsSelected $ \case
          True -> "class" =: "nav-link active"
          False -> "class" =: "nav-link"
    (linkEl, _) <- elDynAttr' "a" highlight $ do
      text $ sectionTitle section
    -- When clicked, go to that section's homepage
    setRoute $ sectionHomepage section <$ domEvent Click linkEl
