{-# LANGUAGE OverloadedStrings #-}
module Frontend.Page.Examples (examples) where

import Reflex.Dom

examples :: DomBuilder t m => m ()
examples = do
  el "ul" $ do
   el "li" $ do
     el "label" $ text "Basic ToDo List: "
     elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-examples/blob/master/BasicTodo/BasicTodo.hs") $ text "See Code Here"
   el "li" $ do
     el "label" $ text "JSON API - NASA Pic of the Day: "
     elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-examples/blob/master/nasa-pod/workshop.hs") $ text "See Code Here"
