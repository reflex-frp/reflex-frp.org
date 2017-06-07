{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts #-}
{-# LANGUAGE GADTs, ScopedTypeVariables, QuasiQuotes, LambdaCase #-}
{-# LANGUAGE MultiWayIf, KindSignatures, ViewPatterns #-}

import Data.Default
import Focus.Backend
import Focus.Backend.Snap
import Snap
import Focus.HTTP.Serve
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
--import Data.Default

import Control.Lens
--import qualified Data.Text.Lazy as TL
--import Lucid
import Frontend.App
import Reflex.Dom.Builder.Static

main :: IO ()
main = do 
  theHead <- fmap snd $ renderStatic siteHead
  theBody <- fmap snd $ renderStatic siteBody
  withFocus . quickHttpServe $ rootHandler theHead theBody

rootHandler :: ByteString -> ByteString -> Snap ()
rootHandler theHead theBody = route 
  [ ("", assetHandler)
  , ("", appHandler theHead theBody)
  ]

assetHandler :: Snap ()
assetHandler = do
  p <- getsRequest rqPathInfo
  if ".html" `BS.isSuffixOf` p || BSC.all (/= '.') p
    then serveAssets "assets" "static"
    --Serve static HTML files without redirecting
    else serveAssets "assets" "static"

-- TODO! Add missing components to app handler, 
-- You should consider re-doing the CSS for the site in Haskell.
appHandler :: ByteString -> ByteString -> Snap ()
appHandler theHead theBody = serveApp "" $ def
  & appConfig_initialHead .~ Just theHead
  & appConfig_initialBody .~ Just (return theBody)
