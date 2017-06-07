{-# LANGUAGE OverloadedStrings #-}

module Common.Route where

import Reflex.Dom
import Data.Text (Text)
import qualified Data.Map as Map
import Frontend.Router 
import Data.Maybe

data Route = Route_Home | Route_Tutorials | Route_Examples | Route_Documentation | Route_FAQ
  deriving (Show)

routeToUrl :: Route -> Text
routeToUrl r = case r of
 Route_Home -> "/home"
 Route_Tutorials -> "/tutorials"
 Route_Examples -> "/examples"
 Route_Documentation -> "/documentation" 
 Route_FAQ -> "/faq"

urlToRoute :: Text -> Maybe Route
urlToRoute path = Map.lookup path routes
  where routes = Map.fromList $ fmap (\r ->(routeToUrl r, r)) [Route_Home , Route_Tutorials , Route_Examples , Route_Documentation , Route_FAQ]

instance IsPath Route where 
  pathToText = routeToUrl 
  textToPath = fromMaybe Route_Home . urlToRoute

