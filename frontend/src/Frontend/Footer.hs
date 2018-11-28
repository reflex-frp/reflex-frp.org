{-# LANGUAGE OverloadedStrings #-}
module Frontend.Footer (footer) where

import Control.Monad (forM_)
import Frontend.FontAwesome
import Reflex.Dom

footer :: DomBuilder t m => m ()
footer = do
  let links =
        [ ("Freenode/#reflex-frp", "http://webchat.freenode.net/?channels=%23reflex-frp&uio=d4")
        , ("Hackage", "https://hackage.haskell.org/package/reflex")
        ]
  forM_ links $ \(name, url) -> elAttr "a" ("href" =: url <> ("target" =: "_blank") <> ("rel" =: "noopener")) $ text name
  let socialIcon i title url = elAttr "a" ("href" =: url <> "title" =: title <> ("rel" =: "noopener")
                                           <> ("target" =: "_blank")) $ brandIcon_ i
  socialIcon "twitter" "Twitter" "https://twitter.com/search?q=%23reflexfrp"
  socialIcon "github" "GitHub" "http://github.com/reflex-frp"
  socialIcon "reddit" "Reddit" "http://reddit.com/r/reflexfrp"
