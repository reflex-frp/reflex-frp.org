{-# LANGUAGE OverloadedStrings #-}
module Frontend.Page.Examples (examples) where

import Reflex.Dom
import Control.Monad (forM_)
import Data.Text (Text)
import Frontend.CommonWidgets

examples :: DomBuilder t m => m ()
examples = do
  el "ul" $ forM_ exList $ \(l, u, s, d) -> do
   el "li" $ do
     el "dt" $ do
       extLink u $ el "label" $ text l
       text " ("
       extLink s $ text "Source"
       text ")"
     el "dd" $ text d
   el "br" blank

exList :: [(Text, Text, Text, Text)]
exList =
  [ ( "reflex-frp.org"
    , "https://reflex-frp.org"
    , "https://github.com/reflex-frp/reflex-frp.org"
    , "This web site"
    )
  , ( "reflex-examples"
    , "https://github.com/reflex-frp/reflex-examples"
    , "https://github.com/reflex-frp/reflex-examples"
    , "Contains a number of simple examples to show reflex usage"
    )
  , ( "Gonimo"
    , "https://gonimo.com"
    , "https://github.com/gonimo/gonimo"
    , "A web based baby monitor, also has an android app"
    )
  , ( "tenjinreader.com"
    , "https://tenjinreader.com"
    , "https://github.com/blueimpact/tenjinreader"
    , "Japanese learning app, also has an android app"
    )
  , ( "Cochlear sound spectrum"
    , "https://cbmm.github.io/cochleagram"
    , "https://github.com/imalsogreg/cochleagram"
    , "Visualize spectrum for sound captured through microphone"
    )
  , ( "HExplore"
    , "http://hexplore.mightybyte.net/"
    , "https://gitlab.com/mightybyte/hexplore/"
    , "Haskell package explorer"
    )
  , ( "fantastic-waddle"
    , "https://github.com/mankyKitty/fantastic-waddle"
    , "https://github.com/mankyKitty/fantastic-waddle"
    , "Generative Art Experiments, makes use of SVG and Canvas"
    )
  , ( "2048 Game"
    , "https://mightybyte.github.io/reflex-2048/"
    , "https://github.com/mightybyte/reflex-2048"
    , ""
    )
  , ( "Flatris Game"
    , "https://rvl.github.io/flatris/"
    , "https://github.com/rvl/flatris"
    , ""
    )
  ]
