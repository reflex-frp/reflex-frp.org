{-# LANGUAGE OverloadedStrings #-}

module Common.Route where

import Data.Text (Text)
import qualified Data.Map as Map
import Data.Maybe

import Frontend.Router 

--Custom data types that correspond to a site's navBar
data Route = Route_Home | Route_Tutorials | Route_Examples | Route_Documentation | Route_FAQ
  deriving (Show)

--Outputs text to be appended to url
routeToUrl :: Route -> Text
routeToUrl r = case r of
 Route_Home -> "/home"
 Route_Tutorials -> "/tutorials"
 Route_Examples -> "/examples"
 Route_Documentation -> "/documentation" 
 Route_FAQ -> "/faq"

--Look up if a given url extension exists within a list of routes
urlToRoute :: Text -> Maybe Route
urlToRoute path = Map.lookup path routes
  where routes = Map.fromList $ fmap (\r ->(routeToUrl r, r)) [Route_Home , Route_Tutorials , Route_Examples , Route_Documentation , Route_FAQ]

-- instance of class from Frontend.Route
instance IsPath Route where 
  pathToText = routeToUrl 
  textToPath = fromMaybe Route_Home . urlToRoute

