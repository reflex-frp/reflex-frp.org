{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts #-}
{-# LANGUAGE GADTs, ScopedTypeVariables, QuasiQuotes, LambdaCase #-}
{-# LANGUAGE MultiWayIf, KindSignatures, ViewPatterns #-}

import Focus.Backend
import Focus.Backend.Snap
import Snap
import Focus.HTTP.Serve
import Reflex.Dom.Builder.Static

import Data.Default
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Text.Encoding
import Control.Lens
import Control.Monad.IO.Class
--import qualified Data.Text as T
--import Data.Maybe

import Frontend.App --used for siteHead & siteBody
import Common.Route --used for urlToRoute function & Route data type

main :: IO ()
main = do 
  theHead <- fmap snd $ renderStatic siteHead
  withFocus . quickHttpServe $ rootHandler theHead 

rootHandler :: ByteString -> Snap ()
rootHandler theHead = route 
  [ ("", assetHandler)
  , ("", do r <- getRequest
            let mRoute = urlToRoute $ decodeUtf8 $ rqURI r
            case mRoute of
              Nothing -> pass
              Just rt -> serveStaticIndex $ def
                & appConfig_initialHead .~ Just theHead
                & appConfig_initialBody .~ Just (liftIO $ fmap snd $ renderStatic $ siteBody rt)
            )
  , ("", appHandler theHead $ fmap snd $ renderStatic $ siteBody $ Route_Home)
  ]

assetHandler :: Snap ()
assetHandler = do
  p <- getsRequest rqPathInfo
  if ".html" `BS.isSuffixOf` p || BSC.all (/= '.') p
    then serveAssets "assets" "static"
    --Serve static HTML files without redirecting
    else serveAssets "assets" "static"

appHandler :: ByteString -> IO ByteString -> Snap ()
appHandler theHead theBody = serveApp "" $ def
  & appConfig_initialHead .~ Just theHead
  & appConfig_initialBody .~ Just (liftIO theBody)
