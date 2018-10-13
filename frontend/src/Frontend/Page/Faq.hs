{-# LANGUAGE OverloadedStrings #-}
module Frontend.Page.Faq (faq) where

import Reflex.Dom

faq :: DomBuilder t m => m ()
faq = do
  elClass "h3" "title" $ do
    text "FAQ"
  el "p" $ do
    text "FAQ questions coming soon! For now, feel free to ask questions within the Reflex-FRP IRC chat provided below. Thank you!"
