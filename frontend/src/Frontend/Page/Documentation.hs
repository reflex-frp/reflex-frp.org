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
module Frontend.Page.Documentation (documentation) where

import Reflex.Dom

documentation :: DomBuilder t m => m ()
documentation = do
  elClass "h3" "title" $ do
    text "Documentation"
  el "ul" $ do
    el "li" $ do
      el "label" $ text "Reflex Basic Documentation: "
      elAttr "a" ("href" =: "https://reflex-frp.readthedocs.io/en/latest/overview.html#reflex-basics") $ text "View Here"
    el "li" $ do
      el "label" $ text "Quick Reference: "
      elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md") $ text "View Here"
