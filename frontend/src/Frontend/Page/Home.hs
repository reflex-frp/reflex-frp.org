{-# LANGUAGE OverloadedStrings #-}
module Frontend.Page.Home (home) where

import Reflex.Dom

home :: DomBuilder t m => m ()
home = do
  elClass "h3" "title" $ do
    text "Practical Functional Reactive Programming"
  elClass "p" "class" $ do
    text "Reflex is a fully-deterministic, higher-order Functional Reactive Programming (FRP) interface and an engine that efficiently implements that interface."
