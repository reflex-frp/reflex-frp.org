{-# LANGUAGE OverloadedStrings #-}
module Frontend.Page.Faq (faq) where

import Reflex.Dom

faq :: DomBuilder t m => m ()
faq = el "p" $
  text "FAQ questions coming soon! For now, feel free to ask questions within the Reflex-FRP IRC chat provided below. Thank you!"
