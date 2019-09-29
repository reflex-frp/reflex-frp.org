{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Frontend.Page.Home (home) where

import Common.Route
import Reflex.Dom
import Obelisk.Route.Frontend
import Obelisk.Generated.Static

home :: (DomBuilder t m, RouteToUrl (R Route) m, SetRoute t (R Route) m) => m ()
home = do
  bigLogo
  slogan
  callToAction
  valueProp
  benefits
  callToAction --TODO: The bottom-of-page call-to-action should probably be different from the top-of-page one

bigLogo :: DomBuilder t m => m ()
bigLogo = do
  elAttr "img" ("src" =: static @"img/logo.svg" <> "alt" =: "Reflex") blank

slogan :: DomBuilder t m => m ()
slogan = do
  el "h1" $ text "The World Changes, Your Apps Should Keep Up."

callToAction :: (DomBuilder t m, RouteToUrl (R Route) m, SetRoute t (R Route) m) => m ()
callToAction = do
  routeLink (Route_GetStarted :/ ()) $ text "Get Started"

valueProp :: DomBuilder t m => m ()
valueProp = do
  el "h2" $ text "Reflex Adapts to Changes in Your..."
  let block title child = do
        el "h3" $ text title
        child
  block "Data" $ do
    el "p" $ text "Reflex apps automatically react to changing data. This keeps every interaction current, accurately representing the relationship between your data and the real world."
    el "p" $ text "Reflex is the key to writing self-updating user interfaces."
  block "Requirements" $ do
    el "p" $ text "Reflex components are modular and reusable. If your requirements change, your app can quickly and easily be reworked. The modularity of Reflex lets you iterate quickly, without wasting code."
    el "p" $ text "Develop efficiently no matter how many times you pivot."
  block "Platform" $ do
    el "p" $ text "Reflex has been built to seamlessly support interfaces on desktop, mobile, web, and other platforms, all in Haskell. Regardless of your platform needs, Reflex lets you take your team and your code with you."
    el "p" $ text "One team, one code base, every platform."

benefits :: DomBuilder t m => m ()
benefits = do
  el "h2" $ text "The Ecosystem Built for Evolution"
  let block title child = do
        el "h3" $ text title
        child
  block "Move Fast Without Breaking Things" $ do
    el "p" $ text "You don’t have to choose between building quickly or sustainably anymore. ReflexFRP allows you to write production quality code from the get-go, with less technical debt."
  block "Never Lost in Translation" $ do
    --TODO: Link to Reflex Platform
    el "p" $ text "Reflex Platform allows you to write entirely in Haskell, everywhere. Using one language does away with client/server protocol mismatches and separate engineering teams. Code becmoes easier to write, with fewer kinds of bugs possible."
  block "Jump the Complexity Wall" $ do
    el "p" $ text "As you add to your codebase it inevitably becomes more complex, but it shouldn’t become harder to manage. No matter the size of your App, Reflex allows for individual pieces of code to be independently understandable."
  block "Code for the Only Constant" $ do
    el "p" $ text "The only constants are time and change, what if your code could account for them? Functional reactive programming lets you write code that understands real world dynamics as naturally as you do, but isn’t any harder to write."
  block "Batteries Included" $ do
    --TODO: Link to Obelisk
    el "p" $ text "Obelisk is the full-stack framework for building Reflex apps using the entire ecosystem. Designed to work ‘out of the box’, with best practices and starter templates included. This is the best option for developing and deploying serious real world applications, quickly."
