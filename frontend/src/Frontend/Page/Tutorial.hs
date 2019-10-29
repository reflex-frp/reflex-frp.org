{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Frontend.Page.Tutorial (tutorial) where

import Obelisk.Route.Frontend
import Reflex.Dom
import Frontend.CommonWidgets

import Common.Route

tutorial :: (DomBuilder t m, RouteToUrl (R Route) m, SetRoute t (R Route) m, Prerender js t m) => Section m
tutorial = Section
  { _section_title = "Tutorial"
  , _section_content = do
    unfinished "TODO" $ text "TODO"
  , _section_subsections = []
  }
