{-# LANGUAGE OverloadedStrings #-}
import Data.Default
import Focus.Backend
import Focus.Backend.Snap
import Snap

main :: IO ()
main = withFocus . quickHttpServe $ rootHandler

rootHandler :: Snap ()
rootHandler =
  route [ ("", serveApp "" $ def)
  ]
