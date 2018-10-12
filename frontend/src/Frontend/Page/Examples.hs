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
module Frontend.Page.Examples (examples) where

import Reflex.Dom

examples :: DomBuilder t m => m ()
examples = do
  elClass "h3" "title" $ text "Check Out Some Example Code"
  el "ul" $ do
   el "li" $ do
     el "label" $ text "Basic ToDo List: "
     elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-examples/blob/master/BasicTodo/BasicTodo.hs") $ text "See Code Here"
   el "li" $ do
     el "label" $ text "JSON API - NASA Pic of the Day: "
     elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-examples/blob/master/nasa-pod/workshop.hs") $ text "See Code Here"
