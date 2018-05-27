{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import Reflex.Dom.Core
import Data.Monoid
import Control.Monad

import Common.Api
import Static

frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    head' = do
      el "title" $ text "Reflex FRP"
      elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: static @"style.css") blank

body :: MonadWidget t m => m ()
body = do
  let links = [ ("hackage", "https://hackage.haskell.org/package/reflex")
              , ("twitter", "http://twitter.com")
              , ("github", "http://github.com/reflex-frp")
              , ("reddit", "http://reddit.com/r/reflexfrp")
              , ("irc.freenode.net #reflex-frp", "http://webchat.freenode.net/?channels=%23reflex-frp&uio=d4")
              ]

  elClass "div" "header" $ do
    elClass "h1" "logo" $ do
      elAttr "img" ("src" =: static @"reflex-frp-logo.jpg") (text "")
    elClass "ul" "sections" $ navMenu

  elClass "div" "main" $ do
    elClass "h3" "title" $ text "Practical Functional Reactive Programming"
    elClass "p" "class" $ text "Reflex is an fully-deterministic, higher-order Functional Reactive Programming (FRP) interface and an engine that efficiently implements that interface."


    -- Create a list of links from a list of tuples
    forM_ links $ \pair -> do
      elAttr "a" ("href" =: (snd pair)) $ text (fst pair)
      el "br" $ return ()

  return ()


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
