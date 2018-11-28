{-# LANGUAGE OverloadedStrings #-}
module Frontend.CommonWidgets where

import Reflex.Dom
import Data.Text (Text)

extLink :: DomBuilder t m => Text -> m a -> m a
extLink href m =
  elAttr "a" ("href" =: href <> "target" =: "_blank" <> "rel" =: "noopener") $ m
