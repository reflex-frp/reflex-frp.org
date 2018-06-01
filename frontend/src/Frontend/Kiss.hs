{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend.Kiss  where

import Reflex
import Reflex.Dom.Core

import Control.Monad.Fix
import Control.Monad

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid

import Frontend.FontAwesome as FA

import Common.Route

--IsPath Modules---
import Language.Javascript.JSaddle
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.Location as Location
import qualified GHCJS.DOM.History as History
import qualified GHCJS.DOM.WindowEventHandlers as DOM
import GHCJS.DOM.Types (Location)
import qualified GHCJS.DOM.Types as DOM
import Control.Lens
--------------------------------

--  | instance of class from Frontend.Route
instance IsPath Route where
  pathToText = routeToUrl
  textToPath = fromMaybe Route_Home . urlToRoute

instance WebRoute Route where
  --  Outputs text to be appended to url
  routeToUrl :: Route -> Text
  routeToUrl r = case r of
   Route_Home -> "/home"
   Route_Tutorials -> "/tutorials"
   Route_Examples -> "/examples"
   Route_Documentation -> "/documentation"
   Route_FAQ -> "/faq"
   --  Look up if a given url extension exists within a list of routes
  urlToRoute :: Text -> Maybe Route
  urlToRoute path = Map.lookup path routes
    where routes = Map.fromList $ fmap (\r ->(routeToUrl r, r)) [Route_Home , Route_Tutorials , Route_Examples , Route_Documentation , Route_FAQ]

  routeToTitle :: Route -> Text
  routeToTitle r = case r of
   Route_Home -> "Home"
   Route_Tutorials -> "Tutorials"
   Route_Examples -> "Examples"
   Route_Documentation -> "Documentation"
   Route_FAQ -> "Faq"

  routeToWidget :: DomBuilder t m => Route -> m ()
  routeToWidget r = case r of
   Route_Home -> home
   Route_Tutorials -> tutorials
   Route_Examples -> examples
   Route_Documentation -> documentation
   Route_FAQ -> faq

-- Set of functions to handle website directories/routes
class WebRoute a where
  routeToTitle :: a -> Text -- returns text to be printed on a Nav Bar's Dom
  routeToUrl :: a -> Text -- returns text to be appended to the URI
  routeToWidget :: (DomBuilder t m, MonadFix m, MonadHold t m) => a -> m () -- returns the corresponding widget
  urlToRoute :: Text -> Maybe a -- opposite of routeToUrl

-- Body generating function, adds navbar and corresponding widgets
bodyGen :: (DomBuilder t m, PostBuild t m, Prerender js m, MonadHold t m
          , MonadFix m, PerformEvent t m, TriggerEvent t m, WebRoute a, IsPath a, Ord a)
              => Text  -- path to image in project directory
              -> [a]   -- list of directories/routes of website
              -> a     -- directory/route website starts on initially
              ->  m ()
bodyGen theLogo pageTabs ir = do
  rec
    pageSwitch <- elClass "div" "header" $ do
      rec
        (homeEvent,_) <- elAttr' "img" ("class" =: "logo" <> "src" =: theLogo) blank
      goHome <- return $ (head pageTabs) <$ domEvent Click homeEvent -- go Home if site logo is clicked
      goNavi <- mobileNavMenu (navMenu active pageTabs) active
      return (leftmost [goHome, goNavi])

    active <- prerender (routeToWidget ir >> return (constDyn ir))
      (pathWidget $ \r -> do
        routeToWidget r
        return (pageSwitch, r))
  return ()

----------------------------------------------------NAV MENU BUILDER ---------------------------------------------------
-- Nav Bar generator produces click-able Widget Events
navMenu :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m, WebRoute a, IsPath a, Ord a) => Dynamic t a -> [a] -> m (Event t a)
navMenu currentTab tabList = do
  let currentTabDemux = demux currentTab      -- change type (Dynamic t a) to (Demux t a)
  rec events <- forM tabList $ \route -> do
        let selected = demuxed currentTabDemux route -- compare currentTab and section
        let highlight = zipDynWith isActive currentTab selected -- if selected is True, highlight currentTab
        el "li" $ do
          -- Get anchor tag element with Route name and corresponding "active:" styling
          (linkEl, _) <- elDynAttr' "a" (highlight) $ text (routeToTitle route)
          return (route <$ domEvent Click linkEl)  -- get Event t Route anchor element
  -- send clicked Route to \route function
  return $ leftmost events

isActive :: (WebRoute a) => a -> Bool -> Map Text Text
isActive ia isit = "id" =: (routeToTitle ia)
           <> "class" =: (active isit)
  where
    active True = "chosenOne"
    active False = ""

-------------MOBILE NAV MENU BUILDER ----------------------------------
--TODO some of the style changes that are within the style.css file may want
--to be integrated into the function somehow in order to avoid fingering
--through css code to figure out how to get this function to be useful
--straight out of the box.
-- Make the mobile app Menu become the parent ul of the li generated from
-- 'navMenu'
mobileNavMenu :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, WebRoute a, IsPath a, Ord a)=> m (Event t a) -> Dynamic t a -> m (Event t a)
mobileNavMenu items activeTab = do
  rec
    isOpen <- toggle False onClick                      -- add toggle-able Boolean Event when clicked
    let toggleOpen = section <$> isOpen                 -- fmap Boolean to 'section'
    let onClick = domEvent Click modalDiv                -- add Event target
    (modalDiv,mWidg) <- elDynAttr' "div" toggleOpen $ do -- Bootleg modal div (used to close dropdown if user clicks elsewhere)
      (_,widg) <- elDynAttr' "ul" toggleOpen $ do        -- get a tuple with (EventResult, m())
        let selectedTitle = Text.toUpper . routeToTitle <$> activeTab    -- set Title for Responsive Menu
        el "div" $ text " "                              -- added this div for flexbox fix (temp fix)
        el "p" $ dynText selectedTitle                   -- add h3 with Dynmically changing title
        _ <- FA.faIcon' FaBars $ def -- add FontAwsome Menu Icon with Large size configs added
        items                                             -- add contents of whatever widget is passed as an arg
      return widg
  return (mWidg)

-- helper function for mobileNavMenu
section :: Bool -> Map Text Text
section True = "class" =: "sections"
section False = "class" =: "noshow"

--------------------TODO everything below should have it's own module or be put elsewhere--------

-- This class provides a mapping between the path portion of a URL encoded as Text, and an arbitrary data type.
-- the expectation being that you'll define an algebraic data type for the possible paths, and map invalid paths to
-- some particular value of that (perhaps representing "home" or a "404").
class IsPath path where
  pathToText :: path -> Text
  textToPath :: Text -> path

instance IsPath Text where
  pathToText = id
  textToPath = id

-- Return the appended path, search and hash excluding hostname or port.
getPath :: MonadJSM m => Location -> m Text
getPath location = do
  pathname <- Location.getPathname location
  search <- Location.getSearch location
  hash <- Location.getHash location
  return $ mconcat [pathname, search, hash]

-- FIXME decodeURI may throw
decodeURI :: (MonadJSM m, ToJSString a, FromJSString b) => a -> m b
decodeURI input = do
  window <-  DOM.currentWindowUnchecked
  window' <- DOM.liftJSM $ toJSVal window
  DOM.liftJSM $ window' ^. js1 ("decodeURI"::Text) input >>= fromJSValUnchecked

-- encodeURI is used for full urls and components only for segments like search, hash, or path
getPathDecoded :: MonadJSM m => Location -> m Text
getPathDecoded = decodeURI <=< getPath

browserHistoryText :: (MonadJSM m, TriggerEvent t m) => m (Text, Event t Text)
browserHistoryText = do
  window <- DOM.currentWindowUnchecked
  location <- Window.getLocation window
  loc0 <- getPathDecoded location
  loc <- wrapDomEvent window (`DOM.on` DOM.popState) $ getPathDecoded location
  return (loc0, loc)

browserHistory :: (IsPath p, MonadJSM m, TriggerEvent t m, Reflex t) => m (p, Event t p)
browserHistory = do
  (location, locationEvent) <- browserHistoryText
  return (textToPath location, fmap textToPath locationEvent)

-- Constructs a widget which switches according to the current browser location.
-- It takes a function which, given the current path, produces the widget to be displayed,
-- and this widget produces, along with its ordinary result, an Event that, when it occurs
-- will cause the browser history to be pushed (and the URL updated), as well as the widget
-- to be switched accordingly.
-- The overall widget produces a Dynamic result whose value is the result of the currently
-- displayed widget.
pathWidget :: ( IsPath p
              , DomBuilder t m
              , TriggerEvent t m
              , PerformEvent t m
              , MonadHold t m
              , MonadFix m
              , MonadJSM m
              , MonadJSM (Performable m))
           => (p -> m (Event t p, a))
            -- Function which, given a path, will construct the widget to be displayed. This widget
            -- additionally returns an Event which, when it fires, sets the path (updating the browser history)
            -- and causes the widget to be rebuilt with the new path.
           -> m (Dynamic t a)
pathWidget f = do
  (path0, pathUpdate) <- browserHistory
  let pageFor path = Workflow $ do
        (switchE, x) <- f path -- run the child widget, getting an Event that tells us when it would like to switch paths, and a result
        performEvent_ . ffor switchE $ \newPath -> pushState' (pathToText newPath)
        return (x, fmap pageFor (leftmost [switchE, pathUpdate]))
  workflow (pageFor path0)

-- Wrapper for pushState that doesn't require state or title.
pushState' :: MonadJSM m => Text -> m ()
pushState' r = do
  history <- Window.getHistory =<< DOM.currentWindowUnchecked
  History.pushState history () (""::Text) $ Just r

