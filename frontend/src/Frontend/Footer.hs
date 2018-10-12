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
module Frontend.Footer (footer) where

import Control.Monad (forM_)
import Data.Map (Map)
import Data.Text
import qualified Frontend.FontAwesome as FA
import Reflex.Dom

footer :: DomBuilder t m => m ()
footer = do
  elClass "div" "main" $ do
    el "p" $ text "Check us out on Hackage or join the community IRC chat!"
    let links =
          [ ("Hackage", "https://hackage.haskell.org/package/reflex")
          , ("irc.freenode.net #reflex-frp", "http://webchat.freenode.net/?channels=%23reflex-frp&uio=d4")
          ]
    forM_ links $ \pair -> do
      elAttr "a" ("href" =: (snd pair)) $ text (fst pair)
      el "br" $ return ()
  el "br" blank

  --  Place Font Awesome Icons in footer <div>
  elClass "div" "footer" $ do
    elAttr "a" twitterAttrs $ do
      FA.faIcon FA.FaTwitter def
    elAttr "a" githubAttrs $ do
      FA.faIcon FA.FaGithub def
    elAttr "a" redditAttrs $ do
      FA.faIcon FA.FaReddit def

twitterAttrs :: Map Text Text
twitterAttrs = "href" =: "https://twitter.com/search?q=%23reflexfrp"
            <> "title" =: "twitter"

githubAttrs :: Map Text Text
githubAttrs = "href" =: "http://github.com/reflex-frp"
           <> "title" =: "github"

redditAttrs :: Map Text Text
redditAttrs = "href" =: "http://reddit.com/r/reflexfrp"
           <> "title" =: "reddit"
