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
module Frontend.Page.Faq (faq) where

import Reflex.Dom

faq :: DomBuilder t m => m ()
faq = elClass "div" "main" $ do
  elClass "h3" "title" $ text "FAQ"
  el "p" $ text "FAQ questions coming soon! For now, feel free to ask questions within the Reflex-FRP IRC chat provided below. Thank you!"
