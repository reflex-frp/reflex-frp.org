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
module Frontend.Page.Tutorials (tutorials) where

import Reflex.Dom

tutorials :: DomBuilder t m => m ()
tutorials = elClass "div" "main" $ do
  elClass "h3" "title" $ text "Tutorials"
  el "ol" $ do
    el "li" $ do
      el "label" $ text "Installation: "
      elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-platform/blob/develop/README.md") $ text "setup-instructions"
    el "li" $ do
      el "label" $ text "Beginner Friendly Tutorial: "
      elAttr "a" ("href" =: "https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md") $ text "reflex-dom-inbits"
