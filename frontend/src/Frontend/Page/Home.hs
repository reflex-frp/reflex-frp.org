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
module Frontend.Page.Home (home) where

import Reflex.Dom

home :: DomBuilder t m => m ()
home = elClass "div" "main" $ do
  elClass "h3" "title" $ text "Practical Functional Reactive Programming"
  elClass "p" "class" $ text "Reflex is an fully-deterministic, higher-order Functional Reactive Programming (FRP) interface and an engine that efficiently implements that interface."
