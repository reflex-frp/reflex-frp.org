{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecursiveDo, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Common.Route where

import Reflex.Dom

import Data.Text (Text)
import qualified Data.Map as Map
import Data.Maybe

import Frontend.Kiss

-- | Custom data type that corresponds to a site's navBar
data Route = Route_Home | Route_Tutorials | Route_Examples | Route_Documentation | Route_FAQ
  deriving (Show, Eq, Ord)

--  | instance of class from Frontend.Route
instance IsPath Route where
  pathToText = routeToUrl
  textToPath = fromMaybe Route_Home . urlToRoute

instance WebRoute Route where
  -- | Outputs text to be appended to url
  routeToUrl :: Route -> Text
  routeToUrl r = case r of
   Route_Home -> "/home"
   Route_Tutorials -> "/tutorials"
   Route_Examples -> "/examples"
   Route_Documentation -> "/documentation"
   Route_FAQ -> "/faq"

  -- | Look up if a given url extension exists within a list of routes
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

-----------------------------------------WIDGET BODIES------------------------------------
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
