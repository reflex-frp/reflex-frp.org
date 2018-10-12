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
import Control.Monad.Fix
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Universe
import qualified Frontend.FontAwesome as FA
import Obelisk.Route.Frontend
import Reflex.Dom

-------------MOBILE NAV MENU BUILDER ----------------------------------
--TODO some of the style changes that are within the style.css file may want
--to be integrated into the function somehow in order to avoid fingering
--through css code to figure out how to get this function to be useful
--straight out of the box.
-- Make the mobile app Menu become the parent ul of the li generated from
-- 'navMenu'
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
  activeTab <- askRoute
  rec isOpen <- toggle False $ domEvent Click modalButtonAndBackground
      let openAttrs = ffor isOpen $ \case
            True -> "class" =: "sections"
            False -> "class" =: "noshow"
      (modalButtonAndBackground, _) <- elDynAttr' "div" openAttrs $ do
        elDynAttr "ul" openAttrs $ do
          let selectedTitle = routeToTitle <$> activeTab
          el "div" $ text " "
          el "p" $ dynText selectedTitle
          _ <- FA.faIcon' FA.FaBars $ def
          navMenu
  return ()

-- Nav Bar generator produces click-able Widget Events
navMenu
  :: ( DomBuilder t m
     , PostBuild t m
     , SetRoute t (R Route) m
     , Routed t (R Route) m
     , Universe (Some Route)
     )
  => m ()
navMenu = do
  currentTab <- askRoute
  let currentTabDemux = demux currentTab      -- change type (Dynamic t a) to (Demux t a)
  forM_ universe $ \(Some.This sec) -> do
    let route = sec :/ case sec of
          Route_Home -> ()
          Route_Tutorials -> ()
          Route_Examples -> ()
          Route_Documentation -> ()
          Route_FAQ -> ()
        selected = demuxed currentTabDemux route -- compare currentTab and section
        highlight = ffor selected $ \case
          True -> "class" =: "chosenOne"
          False -> mempty
    el "li" $ do
      -- Get anchor tag element with Route name and corresponding "active:" styling
      (linkEl, _) <- elDynAttr' "a" highlight $ text $ routeToTitle route
      setRoute $ route <$ domEvent Click linkEl
