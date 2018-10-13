{-# LANGUAGE OverloadedStrings #-}
module Frontend.Page.Tutorials (tutorials) where

import Reflex.Dom

tutorials :: DomBuilder t m => m ()
tutorials = do
  elClass "h3" "title" $ do
    text "Tutorials"
  el "ol" $ do
    el "li" $ do
      el "label" $ text "Installation: "
      elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-platform/blob/develop/README.md") $ text "setup-instructions"
    el "li" $ do
      el "label" $ text "Beginner Friendly Tutorial: "
      elAttr "a" ("href" =: "https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md") $ text "reflex-dom-inbits"
