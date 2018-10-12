{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module Frontend.Nav (nav) where

import Common.Route
import Control.Monad (forM_)
import Control.Monad.Fix (MonadFix)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Universe (Universe, universe)
import qualified Frontend.FontAwesome as FA
import Obelisk.Route.Frontend (Routed (askRoute), R, SetRoute (setRoute))
import Reflex.Dom

-- | Build the entire nav bar, with hamburger menu for expanding on mobile
nav
  :: ( DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     , Routed t (R Route) m
     , SetRoute t (R Route) m
     )
  => m ()
nav = do
  rec isOpen <- toggle False $ domEvent Click modalButtonAndBackground
      let openAttrs = ffor isOpen $ \case
            True -> "class" =: "sections"
            False -> "class" =: "noshow"
      (modalButtonAndBackground, _) <- elDynAttr' "div" openAttrs $ do
        elDynAttr "ul" openAttrs $ do
          -- Build the title items, which will only be displayed on small screens
          --TODO: these elements really shouldn't be inside the `ul`
          el "div" $ text " "
          activeTab <- askRoute
          el "p" $ dynText $ routeTitle <$> activeTab
          _ <- FA.faIcon' FA.FaBars $ def

          -- Build the actual tabs
          menu
  return ()

-- | Build the nav's tabs
menu
  :: ( DomBuilder t m
     , PostBuild t m
     , SetRoute t (R Route) m
     , Routed t (R Route) m
     , Universe (Some Route)
     )
  => m ()
menu = do
  -- Get the current route, so that we can highlight the corresponding tab
  currentTab <- askRoute
  let currentTabDemux = demux $ fmap (\(sec :=> _) -> Some.This sec) currentTab
  -- Iterate over all the top-level routes
  forM_ universe $ \section -> el "li" $ do
    -- Create a link that is highlighted if it is the current section
    let thisTabIsSelected = demuxed currentTabDemux section
        highlight = ffor thisTabIsSelected $ \case
          True -> "class" =: "chosenOne"
          False -> mempty
    (linkEl, _) <- elDynAttr' "a" highlight $ do
      text $ sectionTitle section
    -- When clicked, go to that section's homepage
    setRoute $ sectionHomepage section <$ domEvent Click linkEl
