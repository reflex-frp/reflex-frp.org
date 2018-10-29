{-# LANGUAGE OverloadedStrings #-}
module Frontend.Page.Examples (examples) where

import Reflex.Dom

examples :: DomBuilder t m => m ()
examples = do
  el "ul" $ do
   el "li" $ do
     el "label" $ text "Basic ToDo List: "
     elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-examples/tree/master/BasicTodo") $ text "See Code Here"
   el "li" $ do
     el "label" $ text "JSON API - NASA Pic of the Day: "
     elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-examples/tree/master/nasa-pod") $ text "See Code Here"
