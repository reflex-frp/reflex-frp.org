{-# LANGUAGE RecursiveDo, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Reflex
import Reflex.Dom
import qualified Data.Map as Map
import Safe (readMay)
import Control.Applicative ((<*>), (<$>))

import Data.FileEmbed
import Data.Monoid
import Control.Monad
import Data.Map (Map)
import Debug.Trace
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = mainWidgetWithHead siteHead siteBody

siteHead :: MonadWidget t m => m ()
siteHead = do
  el "title" $ text "Reflex FRP"
  let fa = "font-awesome-4.7.0/css/font-awesome.min.css"
  headLink fa
  styleSheet "style.css"
  return ()

siteBody :: MonadWidget t m => m ()
siteBody = do 
  let links = [ ("hackage", "https://hackage.haskell.org/package/reflex")
              , ("twitter", "http://twitter.com")
              , ("github", "http://github.com/reflex-frp")
              , ("reddit", "http://reddit.com/r/reflexfrp")
              , ("irc.freenode.net #reflex-frp", "http://webchat.freenode.net/?channels=%23reflex-frp&uio=d4")
              ]

  elClass "div" "header" $ do
    elAttr "img" logo blank
    elClass "ul" "sections" $ navMenu

  elClass "div" "main" $ do
    elClass "h3" "title" $ text "Practical Functional Reactive Programming"
    elClass "p" "class" $ text "Reflex is an fully-deterministic, higher-order Functional Reactive Programming (FRP) interface and an engine that efficiently implements that interface."


    -- Create a list of links from a list of tuples
    forM_ links $ \pair -> do
      elAttr "a" ("href" =: (snd pair)) $ text (fst pair)
      el "br" $ return ()
  el "br" blank
  return ()

--Element Attributes
logo :: Map Text Text
logo = "class" =: "logo" 
        <> "src" =: "img/REFLEX.png" 
        <> "style" =: "height: 20%;width: 30%;margin: auto;display: block;padding: 0;"

navMenu :: (MonadWidget t m) => m ()
navMenu = do
  forM_ sections $ \pair -> do
    el "li" $
      elAttr "a" ("href" =: (snd pair)) $ text (fst pair)
  where sections = [ ("Home", "/")
                 , ("Tutorial", "https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md")
                 , ("Examples", "/")
                 , ("Documentation", "http://reflex-frp.readthedocs.io")
                 , ("FAQ", "/")]

--Helper Functions
styleSheet myLink = elAttr "link" (Map.fromList [
    ("rel", "stylesheet"),
    ("type", "text/css"),
    ("href", myLink)
  ]) $ return ()

headLink url = elAttr "link" (Map.fromList [
    ("rel", "stylesheet"),
    ("href", url)
  ]) $ return ()
