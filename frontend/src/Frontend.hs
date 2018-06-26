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
module Frontend where

import Prelude hiding ((.), id, fst, snd)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core
import Data.Monoid
import Control.Monad
import Control.Monad.Fix
import Obelisk.Route

import Common.Api
import Common.Route
import Frontend.Kiss
import qualified Frontend.FontAwesome as FA
import Static

import Reflex.View.Base
import Network.URI
import Control.Category
import qualified Control.Categorical.Functor as Cat
import Control.Categorical.Bifunctor
import Control.Category.Cartesian
import Language.Javascript.JSaddle
import Control.Monad.Reader
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Compose
import Data.GADT.Compare
import Data.Functor.Identity

frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    head' :: DomBuilder t m => m ()
    head' = do
      el "title" $ text "Reflex FRP"      -- add Page title
      elAttr "meta" metaDesc blank        -- add meta-data description
      elAttr "meta" metaKeywords blank    -- add meta-data keywords
      elAttr "meta" viewport blank        -- add meta-data viewport
      FA.fontAwesomeCDN
      -- add various favIcon links
      faviconLinker "icon" "image/png" "16x16" "img/favicon-16x16.png"
      faviconLinker "icon" "image/png" "32x32" "img/favicon-32x32.png"
      faviconLinker "apple-touch-icon" "/" "57x57" "img/apple-touch-icon-57x57.png"
      faviconLinker "apple-touch-icon" "/" "60x60" "img/apple-touch-icon-60x60.png"
      faviconLinker "apple-touch-icon" "/" "72x72" "img/apple-touch-icon-72x72.png"
      faviconLinker "apple-touch-icon" "/" "76x76" "img/apple-touch-icon-76x76.png"
      faviconLinker "apple-touch-icon" "/" "114x114" "img/apple-touch-icon-114x114.png"
      faviconLinker "apple-touch-icon" "/" "120x120" "img/apple-touch-icon-120x120.png"
      faviconLinker "apple-touch-icon" "/" "144x144" "img/apple-touch-icon-144x144.png"
      faviconLinker "apple-touch-icon" "/" "152x152" "img/apple-touch-icon-152x152.png"
      faviconLinker "icon" "image/png" "img/favicon-196x196.png" "196x196"
      styleSheet "style.css"              --  link css stylesheet
      styleSheet "font.css"               --  link css fonts
      return ()

body :: (DomBuilder t m
        , MonadHold t m
        , MonadFix m
        , TriggerEvent t m
        , PostBuild t m
        , PerformEvent t m
        , Prerender x m
        , MonadJSM m
        , MonadJSM (Performable m)
        )
        => m ()
body = do
  runRouteViewT $ bodyGen siteLogo siteRoutes
  elClass "div" "main" $ do
    el "p" $ text "Check us out on Hackage or join the community IRC chat!"
    forM_ links $ \pair -> do
      elAttr "a" ("href" =: (snd pair)) $ text (fst pair)
      el "br" $ return ()
  el "br" blank

  --  Place Font Awesome Icons in footer <div>
  elClass "div" "footer" $ do
    elAttr "a" rdirTwitter $ do
      FA.faIcon FA.FaTwitter def
    elAttr "a" rdirGithub $ do
      FA.faIcon FA.FaGithub def
    elAttr "a" rdirReddit $ do
      FA.faIcon FA.FaReddit def
  where
    siteLogo = "img/REFLEX.png"
    siteRoutes = (:/ ()) <$>
      [ Route_Home
      , Route_Tutorials
      , Route_Examples
      , Route_Documentation
      , Route_FAQ
      ]
    links = [ ("Hackage", "https://hackage.haskell.org/package/reflex")
              , ("irc.freenode.net #reflex-frp", "http://webchat.freenode.net/?channels=%23reflex-frp&uio=d4")
              ]

metaDesc :: Map Text Text
metaDesc = "name" =: "description"
        <> "content" =: "Reflex Functional Reactive Programming"

metaKeywords :: Map Text Text
metaKeywords = "name" =: "keywords"
            <> "content" =: "reflex, reflex frp, functional reactive programming, haskell, framework, reflex dom"

viewport :: Map Text Text
viewport = "name" =: "viewport"
        <> "content" =: "width=device-width, initial-scale=1"

-- TODO: make this function more type safe.
-- The 4 arguments are as follows: rel type size href
-- turn the second argument into a Maybe Text
faviconLinker :: DomBuilder t m => Text -> Text -> Text -> Text -> m ()
faviconLinker r t s h = elAttr "link" attribs blank
    where
      attribs = "rel" =: r
             <> "type" =: t
             <> "size" =: s
             <> "href" =: h

--  styleSheet are functions to add links to html <head>
styleSheet :: DomBuilder t m => Text -> m ()
styleSheet myLink = elAttr "link" (Map.fromList [
    ("rel", "stylesheet"),
    ("type", "text/css"),
    ("href", myLink)
  ]) $ return ()

rdirTwitter :: Map Text Text
rdirTwitter = "href" =: "https://twitter.com/search?q=%23reflexfrp"
           <> "title" =: "twitter"
rdirGithub :: Map Text Text
rdirGithub = "href" =: "http://github.com/reflex-frp"
           <> "title" =: "github"
rdirReddit :: Map Text Text
rdirReddit = "href" =: "http://reddit.com/r/reflexfrp"
           <> "title" =: "reddit"

home :: (DomBuilder t m) => m ()
home = elClass "div" "main" $ do
         elClass "h3" "title" $ text "Practical Functional Reactive Programming"
         elClass "p" "class" $ text "Reflex is an fully-deterministic, higher-order Functional Reactive Programming (FRP) interface and an engine that efficiently implements that interface."

tutorials :: (DomBuilder t m) => m ()
tutorials = elClass "div" "main" $ do
    elClass "h3" "title" $ text "Tutorials"
    el "ol" $ do
      el "li" $ do
        el "label" $ text "Installation: "
        elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-platform/blob/develop/README.md") $ text "setup-instructions"
      el "li" $ do
        el "label" $ text "Beginner Friendly Tutorial: "
        elAttr "a" ("href" =: "https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md") $ text "reflex-dom-inbits"



examples :: (DomBuilder t m) => m ()
examples = elClass "div" "main" $ do
     elClass "h3" "title" $ text "Check Out Some Example Code"
     el "ul" $ do
      el "li" $ do
        el "label" $ text "Basic ToDo List: "
        elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-examples/blob/master/BasicTodo/BasicTodo.hs") $ text "See Code Here"
      el "li" $ do
        el "label" $ text "JSON API - NASA Pic of the Day: "
        elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-examples/blob/master/nasa-pod/workshop.hs") $ text "See Code Here"


documentation :: (DomBuilder t m) => m ()
documentation = elClass "div" "main" $ do
    elClass "h3" "title" $ text "Refreshing Reflex Documentation"
    el "ul" $ do
      el "li" $ do
        el "label" $ text "Reflex Basic Documentation: "
        elAttr "a" ("href" =: "http://reflex-frp.readthedocs.io/en/latest/architecture.html#overview-of-reflex-basics") $ text "View Here"
      el "li" $ do
        el "label" $ text "Quick Reference: "
        elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md") $ text "View Here"


faq :: (DomBuilder t m) => m ()
faq = elClass "div" "main" $ do
            elClass "h3" "title" $ text "FAQ"
            el "p" $ text "FAQ questions coming soon! For now, feel free to ask questions within the Reflex-FRP IRC chat provided below. Thank you!"

routeToTitle :: R Route -> Text
routeToTitle r = case r of
  Route_Home :/ () -> "Home"
  Route_Tutorials :/ () -> "Tutorials"
  Route_Examples :/ () -> "Examples"
  Route_Documentation :/ () -> "Documentation"
  Route_FAQ :/ () -> "Faq"

--TODO: Factor out into Obelisk
--TODO: When the user manually goes to a URL, see if we can parse it as an AppRoute; if we can, jump straight to it without leaving the app; otherwise let the browser go to it separately.  This should also apply to all links
--TODO: Avoid parsing repeatedly
historyItemToUrlPathCommand
  :: forall a.
     ValidEncoder (Either Text) a PageName
  -> ViewMorphism (R (EitherTag Text a)) HistoryItem (Endo (R (EitherTag Text a))) (Endo HistoryItem)
historyItemToUrlPathCommand encoder = ViewMorphism
  { _viewMorphism_mapCommand = \(Endo update) -> Endo $ \oldVal -> case update $ eitherToDSum $ _validEncoder_decode fullEncoder $ (uriPath &&& uriQuery) $ _historyItem_uri oldVal of
      LeftTag :/ _ -> oldVal --TODO: Do something with errors?
      RightTag :/ newRoute -> case _validEncoder_encode encoder newRoute of
        (newPathSegments, newQueryItems) -> oldVal
          { _historyItem_uri = (_historyItem_uri oldVal)
            { uriPath = _validEncoder_encode pathEncoder newPathSegments
            , uriQuery = _validEncoder_encode queryEncoder newQueryItems
            }
          }
  , _viewMorphism_mapState = eitherToDSum . _validEncoder_decode fullEncoder . (uriPath &&& uriQuery) . _historyItem_uri
  }
  where --TODO: URL component encoding/decoding
    Right pathEncoder = checkEncoder $ unpackTextEncoder . prefixTextEncoder "/" . intercalateTextEncoder "/" . listToNonEmptyEncoder :: Either Text (ValidEncoder (Either Text) [Text] String)
    Right queryEncoder = checkEncoder $ unpackTextEncoder . prefixNonemptyTextEncoder "?" . intercalateTextEncoder "&" . listToNonEmptyEncoder . Cat.fmap (joinPairTextEncoder "=") . toListMapEncoder :: Either Text (ValidEncoder (Either Text) (Map Text Text) String)
    fullEncoder = bimap pathEncoder queryEncoder . encoder

--TODO: Factor this out into Obelisk
runRouteViewT
  :: ( Monad m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadHold t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , MonadFix m
     )
  => ViewT t (R (EitherTag Text (R Route))) (Endo (R (EitherTag Text (R Route)))) m a
  -> m ()
runRouteViewT b = do
  rec historyState <- manageHistory $ HistoryCommand_PushState <$> setState
      let Right encoder = checkEncoder $ obeliskRouteEncoder showRoute routeRestEncoder . Encoder (pure $ prismValidEncoder $ rPrism _ObeliskRoute_App)
      changeState <- execViewT (mapViewT (historyItemToUrlPathCommand encoder) b) historyState
      let f oldItem change =
            let newItem = appEndo change oldItem
            in HistoryStateUpdate
               { _historyStateUpdate_state = _historyItem_state newItem
               , _historyStateUpdate_title = "Reflex FRP" --TODO
               , _historyStateUpdate_uri = Just $ _historyItem_uri newItem
               }
          setState = attachWith f (current historyState) changeState
  return ()

--TODO: This needs to use a monad other than ReaderT - something that guarantees that the value is actually available.  Perhaps a "strict ReaderT"? Otherwise factorDyn will fail
factorRoute_ :: (Reflex t, MonadFix m, MonadHold t m, GEq r, Adjustable t m) => (forall a. r a -> ReaderT (Dynamic t a) m ()) -> ReaderT (Dynamic t (R r)) m ()
factorRoute_ f = ReaderT $ \d -> do
  d' <- factorDyn d
  strictDynWidget_ d' $ \(r :=> Compose d'') -> do
    runReaderT (f r) $ coerceDynamic d''

-- Body generating function, adds navbar and corresponding widgets
bodyGen :: (DomBuilder t m, PostBuild t m, Prerender js m, MonadHold t m
          , MonadFix m, PerformEvent t m, TriggerEvent t m)
              => Text  -- path to image in project directory
              -> [R Route]   -- list of directories/routes of website
              -> ViewT t (R (EitherTag Text (R Route))) (Endo (R (EitherTag Text (R Route)))) m ()
bodyGen theLogo pageTabs = factorRoute_ $ \case
  LeftTag -> text "404" --TODO: Do we actually want a 404 page?
  RightTag -> elClass "div" "header" $ do
    (homeEvent,_) <- elAttr' "img" ("class" =: "logo" <> "src" =: theLogo) blank
    tellEvent $ Endo (const (RightTag :/ head pageTabs)) <$ domEvent Click homeEvent -- go Home if site logo is clicked
    mobileNavMenu $ navMenu pageTabs
    factorRoute_ $ \case
      Route_Home -> home
      Route_Tutorials -> tutorials
      Route_Examples -> examples
      Route_Documentation -> documentation
      Route_FAQ -> faq

----------------------------------------------------NAV MENU BUILDER ---------------------------------------------------
-- Nav Bar generator produces click-able Widget Events
navMenu
  :: ( DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     , EventWriter t (Endo (R (EitherTag Text (R Route)))) m
     , MonadReader (Dynamic t (R Route)) m
     )
  => [R Route]
  -> m ()
navMenu tabList = do
  currentTab <- ask
  let currentTabDemux = demux currentTab      -- change type (Dynamic t a) to (Demux t a)
  forM_ tabList $ \route -> do
        let selected = demuxed currentTabDemux route -- compare currentTab and section
        let highlight = zipDynWith isActive currentTab selected -- if selected is True, highlight currentTab
        el "li" $ do
          -- Get anchor tag element with Route name and corresponding "active:" styling
          (linkEl, _) <- elDynAttr' "a" (highlight) $ text (routeToTitle route)
          tellEvent $ Endo (const $ RightTag :/ route) <$ domEvent Click linkEl

isActive :: R Route -> Bool -> Map Text Text
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
mobileNavMenu
  :: ( DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     , MonadReader (Dynamic t (R Route)) m
     )
  => m ()
  -> m ()
mobileNavMenu items = do
  activeTab <- ask
  rec
    isOpen <- toggle False onClick                      -- add toggle-able Boolean Event when clicked
    let toggleOpen = section <$> isOpen                 -- fmap Boolean to 'section'
    let onClick = domEvent Click modalDiv                -- add Event target
    (modalDiv,mWidg) <- elDynAttr' "div" toggleOpen $ do -- Bootleg modal div (used to close dropdown if user clicks elsewhere)
      (_,widg) <- elDynAttr' "ul" toggleOpen $ do        -- get a tuple with (EventResult, m())
        let selectedTitle = T.toUpper . routeToTitle <$> activeTab    -- set Title for Responsive Menu
        el "div" $ text " "                              -- added this div for flexbox fix (temp fix)
        el "p" $ dynText selectedTitle                   -- add h3 with Dynmically changing title
        _ <- FA.faIcon' FA.FaBars $ def -- add FontAwsome Menu Icon with Large size configs added
        items                                             -- add contents of whatever widget is passed as an arg
      return widg
  return ()

-- helper function for mobileNavMenu
section :: Bool -> Map Text Text
section True = "class" =: "sections"
section False = "class" =: "noshow"
