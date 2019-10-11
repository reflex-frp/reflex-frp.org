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
import Obelisk.Generated.Static
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom
import Data.Text (Text)

import Frontend.CommonWidgets

-- | Build the entire nav bar
nav
  :: ( DomBuilder t m
     , PostBuild t m
     , Routed t (R Route) m
     , RouteToUrl (R Route) m
     , SetRoute t (R Route) m
     )
  => m ()
nav = do
  unfinished "needs logo" $ text "REFLEX" -- TODO ensure the correct colours on each page
  el "nav" menu

-- | Displays the logo and returns an event that fires when the logo is clicked
logo :: (DomBuilder t m, SetRoute t (R Route) m, RouteToUrl (R Route) m) => m ()
logo = do
  let logoAttrs = mconcat
        [ "class" =: "logo"
        , "src" =: static @"img/logo.svg"
        , "alt" =: "Reflex"
        ]
  routeLink (Route_Home :/ ()) $ elAttr "img" logoAttrs blank

-- | Build the nav's tabs
menu
  :: ( DomBuilder t m
     , PostBuild t m
     , SetRoute t (R Route) m
     , Routed t (R Route) m
     , RouteToUrl (R Route) m
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
    elDynClass "span" highlight $ routeLink (sectionHomepage section) $ text $ sectionTitle section

--TODO: Factor out into a library or something
forkMeOnGithub
  :: DomBuilder t m
  => Text
  -> Text
  -> m ()
forkMeOnGithub repoOwner repoName = do
  -- The banner is only shown on the bigger screens on top right corner
  elClass "span" "fork-link" $ extLink href $
    elAttr "img" (("src" =: src) <> ("alt" =: alt)) $ return ()
  -- The inline link is only shown on mobile screens along with other menu options
  elClass "span" "fork-link-inline" $ extLink href $
    text "Fork me on Github"
  where
    href = "https://github.com/" <> repoOwner <> "/" <> repoName
    src = "https://s3.amazonaws.com/github/ribbons/forkme_right_gray_6d6d6d.png"
    alt = "Fork me on GitHub"
