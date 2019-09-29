{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
module Frontend (frontend) where

import Common.Route
import Data.Dependent.Sum (DSum(..))
import Frontend.Head
import Frontend.Nav
import Frontend.Page.Home
import Frontend.Page.GetStarted

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

frontend :: Frontend (R Route)
frontend = Frontend
  { _frontend_head = pageHead
  , _frontend_body = do
      -- The recursion here allows us to send a click event from the content area "up" into the header
      rec el "header" $ nav click
          click <- mainContainer $ do
            article $ subRoute_ $ \case
              Route_Home -> home
              Route_GetStarted -> getStarted
      return ()
  }

-- | The @<main>@ tag that will contain most of the site's content
mainContainer :: DomBuilder t m => m () -> m (Event t ())
mainContainer w = domEvent Click . fst <$> el' "main" w

-- | An @<article>@ tag that will set its title and the class of its child
-- @<section>@ based on the current route
article
  :: ( DomBuilder t m
     )
  => m () -- ^ Article content widget
  -> m ()
article = el "article"
