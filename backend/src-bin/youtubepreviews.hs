{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text (Text)
import Data.Universe
import Network.HTTP
import Network.URI

import Common.Route

main :: IO ()
main = do
  let getUrl (Left extTalk) = externalTalkThumbnailUrl extTalk
      getUrl (Right talk) = (youtubeId, imgUrl)
        where
          youtubeId = talkYoutubeId $ talkHomepage talk
          imgUrl = "http://i3.ytimg.com/vi/" <> youtubeId <> "/hqdefault.jpg"
  forM_ universe $ \talk -> fetchThumbnail (getUrl talk)

fetchThumbnail :: (Text, Text) -> IO ()
fetchThumbnail (imgName, imgUrl) = case parseURI (T.unpack imgUrl) of
  Nothing -> error $ "Couldn't parse preview image url: " <> T.unpack imgUrl
  Just url -> simpleHTTP (mkRequest GET url :: Request BS.ByteString) >>= \case
    Left err -> error $ "Couldn't connect: " <> show err
    Right (Response { rspCode = (2,0,0), rspBody = bs }) -> do
      let resultPath = T.unpack $ "static/img/talk/" <> imgName <> ".jpg"
      BS.writeFile resultPath bs
    Right (Response { rspCode = c, rspReason = r }) -> error $ mconcat
      [ "Bad response: "
      , show c
      , ", "
      , r
      ]
