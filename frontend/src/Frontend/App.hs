{-# LANGUAGE RecursiveDo, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend.App where

import Reflex
import Reflex.Dom
import qualified Data.Map as Map
--import Safe (readMay)
--import Control.Applicative ((<*>), (<$>))

--import Data.FileEmbed
import Data.Monoid
import Control.Monad
import Data.Map (Map)
--import Debug.Trace
import Data.Text (Text)
--import qualified Data.Text as T

siteHead :: DomBuilder t m => m ()
siteHead = do
  el "title" $ text "Reflex FRP"
  elAttr "meta" metaDesc blank
  elAttr "meta" metaKeywords blank
  let fa = "font-awesome-4.7.0/css/font-awesome.min.css"
  headLink fa
  styleSheet "style.css"
  return ()

siteBody :: (DomBuilder t m, MonadHold t m) => m ()
siteBody = do 
  let links = [ ("Hackage", "https://hackage.haskell.org/package/reflex")
              , ("irc.freenode.net #reflex-frp", "http://webchat.freenode.net/?channels=%23reflex-frp&uio=d4")
              ]

  let x = do elClass "div" "main" $ do
              elClass "h3" "title" $ text "Practical Functional Reactive Programming"
              elClass "p" "class" $ text "Reflex is an fully-deterministic, higher-order Functional Reactive Programming (FRP) interface and an engine that efficiently implements that interface."

  
  pageSwitch <- elClass "div" "header" $ do
    elAttr "img" logo blank
    elClass "ul" "sections" $ navMenu
    --try to change some of the nav li elements into events

  _ <- ($) widgetHold x $ pageSwitch

    -- Create a list of links from a list of tuples
  elClass "div" "main" $ do 
    el "p" $ text "Check us out on Hackage or join the community IRC chat!"
    forM_ links $ \pair -> do
      elAttr "a" ("href" =: (snd pair)) $ text (fst pair)
      el "br" $ return ()
  el "br" blank

  elClass "div" "footer" $ do
   elAttr "a" rdirTwitter $ do
    elAttr "i" (("class" =: "fa fa-twitter") <> ("aria-hidden" =: "true")) blank 
   elAttr "a" rdirGithub $ do
    elAttr "i" (("class" =: "fa fa-github") <> ("aria-hidden" =: "true")) blank 
   elAttr "a" rdirReddit $ do
    elAttr "i" (("class" =: "fa fa-reddit") <> ("aria-hidden" =: "true")) blank 
  return ()

--Element Attributes
metaDesc :: Map Text Text
metaDesc = "name" =: "description" 
        <> "content" =: "Reflex Functional Reactive Programming"

metaKeywords :: Map Text Text
metaKeywords = "name" =: "keywords"
            <> "content" =: "reflex, reflex frp, functional reactive programming, haskell, framework, reflex dom" 

logo :: Map Text Text
logo = "class" =: "logo" 
        <> "src" =: "img/REFLEX.png" 
        <> "style" =: "height: 20%;width: 30%;margin: auto;display: block;padding: 0;"

rdirTwitter :: Map Text Text
rdirTwitter = "href" =: "https://twitter.com/search?q=%23reflexfrp"
           <> "title" =: "twitter"
rdirGithub :: Map Text Text
rdirGithub = "href" =: "http://github.com/reflex-frp"
           <> "title" =: "github"
rdirReddit :: Map Text Text
rdirReddit = "href" =: "http://reddit.com/r/reflexfrp"
           <> "title" =: "reddit"

{-
--Nav Bar generator with Anchor tags 
navMenu :: (MonadWidget t m) => m ()
navMenu = do
  forM_ sections $ \pair -> do
    el "li" $
      elAttr "a" ("href" =: (snd pair) <> "id" =: (fst pair)) $ text (fst pair)
  where sections = [ ("Home", "/")
                 , ("Tutorials", "https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md")
                 , ("Examples", "/")
                 , ("Documentation", "http://reflex-frp.readthedocs.io")
                 , ("FAQ", "/")]
-}

--Nav Bar generator with click-able Events
navMenu :: (DomBuilder t m) => m (Event t (m ()))
navMenu = do 
  events <- forM sections $ \(sectionName, section) -> do
    el "li" $ do
      (linkEl, _) <- elAttr' "a" ("id" =: (sectionName)) $ text (sectionName)
      return (section <$ domEvent Click linkEl) 
  return $ leftmost events
  where sections = [ ("Home", home)
                 , ("Tutorials", tutorials)
                 , ("Examples", examples)
                 , ("Documentation", documentation)
                 , ("FAQ", faq)]

--nav bar body components
home :: (DomBuilder t m) => m ()
home = elClass "div" "main" $ do
              elClass "h3" "title" $ text "Practical Functional Reactive Programming"
              elClass "p" "class" $ text "Reflex is an fully-deterministic, higher-order Functional Reactive Programming (FRP) interface and an engine that efficiently implements that interface."


tutorials :: (DomBuilder t m) => m ()
tutorials = elClass "div" "main" $ do
    elClass "h3" "title" $ text "Tutorials:"
    el "ol" $ do
      el "li" $ do 
        el "label" $ text "Installation: "
        elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-platform/blob/develop/README.md") $ text "setup-instructions" 
      el "li" $ do 
        el "label" $ text "Beginner Friendly Tutorial: "
        elAttr "a" ("href" =: "https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md") $ text "reflex-dom-inbits" 



examples :: (DomBuilder t m) => m ()
examples = elClass "div" "main" $ do
     elClass "h3" "title" $ text "Check Out Some Example Code:"
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
            elClass "h3" "title" $ text "FAQ:"
            el "p" $ text "FAQ questions coming soon! For now, feel free to ask questions within the Reflex-FRP IRC chat provided below. Thank you!"

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
