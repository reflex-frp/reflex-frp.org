{-# LANGUAGE OverloadedStrings #-}
module Frontend.Page.Documentation (documentation) where

import Reflex.Dom
import Frontend.CommonWidgets

documentation :: DomBuilder t m => m ()
documentation = do
  el "ul" $ do
    el "li" $ do
      extLink "https://github.com/reflex-frp/reflex/blob/develop/Quickref.md" $ text "Reflex Quick Reference"
    el "li" $ do
      extLink "https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md" $ text "Reflex-Dom Quick Reference"
    el "li" $ do
      extLink "http://docs.reflex-frp.org/en/latest/reflex_docs.html" $ text "Reflex Basic Documentation (work-in-progress)"
