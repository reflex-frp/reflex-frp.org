{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core
import Data.Monoid
import Control.Monad
import Control.Monad.Fix

import Common.Api
import Common.Route
import Frontend.Kiss
import qualified Frontend.FontAwesome as FA
import Static

frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body Route_Home)
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
        , Prerender x m)
        => Route -> m ()
body initRoute = do
  bodyGen siteLogo siteRoutes initRoute
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
    siteRoutes = [ Route_Home
                 , Route_Tutorials
                 , Route_Examples
                 , Route_Documentation
                 , Route_FAQ ]
    links = [ ("Hackage", "https://hackage.haskell.org/package/reflex")
              , ("irc.freenode.net #reflex-frp", "http://webchat.freenode.net/?channels=%23reflex-frp&uio=d4")
              ]

navMenu :: (MonadWidget t m) => m ()
navMenu = forM_ sections $ \pair -> do
  el "li" $
    elAttr "a" ("href" =: (snd pair)) $ text (fst pair)
  where sections = [ ("Home", "#")
                 , ("Tutorial", "#")
                 , ("Examples", "#")
                 , ("Documentation", "http://reflex-frp.readthedocs.io")
                 , ("FAQ", "#")]


navMenu1 :: (MonadWidget t m) => m ()
navMenu1 = do
  forM_ sections $ \link -> do
    el "li" $
      elAttr "a" ("href" =: "#") $
        text link
  where sections = [ "Home", "Tutorials", "Examples", "Documentation", "FAQ"]

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

