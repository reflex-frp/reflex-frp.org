{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Page.Home (home) where

import Control.Lens ((%~))
import Data.Text (Text)
import Data.Proxy
import Common.Route
import Reflex.Dom
import Obelisk.Route.Frontend
import Obelisk.Generated.Static

home :: (DomBuilder t m, RouteToUrl (R Route) m, SetRoute t (R Route) m) => m ()
home = do
  slogan
  callToAction "jumbotron"
  valueProp
  elClass "hr" "short" blank
  benefits
  callToAction "" --TODO: The bottom-of-page call-to-action should probably be different from the top-of-page one

bigLogo :: DomBuilder t m => m ()
bigLogo = do
  elAttr "img" ("src" =: static @"img/logo.svg" <> "alt" =: "Reflex") blank

slogan :: DomBuilder t m => m ()
slogan = do
  elClass "h1" "tagline" $ text "The world changes, your apps should keep up."

callToAction :: (DomBuilder t m, RouteToUrl (R Route) m, SetRoute t (R Route) m) => Text -> m ()
callToAction c = do
  divClass ("call-to-action " <> c) $ routeLink (Route_GetStarted :/ ()) $ text "Checkout the Get Started guide"

valueProp :: DomBuilder t m => m ()
valueProp = do
  el "h1" $ text "Reflex adapts to changes in your..."
  elClass "section" "cards" $ do
    let card icon title child = elClass "article" "card" $ do
          el "h3" $ elClass "i" icon blank >> text title
          child
    card "icon-stats" "Data" $ do
      el "p" $ text "Reflex apps automatically react to changing data. This keeps every interaction current, accurately representing the relationship between your data and the real world."
      el "p" $ text "Reflex is the key to writing self-updating user interfaces."
    card "icon-list" "Requirements" $ do
      el "p" $ text "Reflex components are modular and reusable. If your requirements change, your app can quickly and easily be reworked. The modularity of Reflex lets you iterate quickly, without wasting code."
      el "p" $ text "Develop efficiently no matter how many times you pivot."
    card "icon-devices" "Platform" $ do
      el "p" $ text "Reflex has been built to seamlessly support interfaces on desktop, mobile, web, and other platforms, all in Haskell. Regardless of your platform needs, Reflex lets you take your team and your code with you."
      el "p" $ text "One team, one code base, every platform."

benefits :: DomBuilder t m => m ()
benefits = do
  el "h1" $ text "The Ecosystem Built for Evolution"
  let article side url title child = elAttr "article" ("class" =: ("alternating " <> side) <> "style" =: ("background-image: url(" <> url <> ")")) $ do
        el "h3" $ text title
        child
  article "left" (static @"img/graphics/undraw_speed_test_wxl0.svg") "Move fast without breaking things." $ do
    el "p" $ text "You don’t have to choose between building quickly or sustainably anymore. Reflex-FRP allows you to write production quality code from the get-go, with less technical debt."
  article "right" (static @"img/graphics/undraw_code_typing_7jnv.svg") "Never lost in translation." $ do
    --TODO: Link to Reflex Platform
    el "p" $ text "Reflex Platform allows you to write entirely in Haskell, everywhere. Using one language does away with client/server protocol mismatches and separate engineering teams. Code becomes easier to write, with fewer kinds of bugs possible."
  article "left" (static @"img/graphics/undraw_good_team_m7uu.svg") "Jump the complexity wall." $ do
    el "p" $ text "As you add to your codebase it inevitably becomes more complex, but it shouldn’t become harder to manage. No matter the size of your App, Reflex allows for individual pieces of code to be independently understandable."
  article "right" (static @"img/graphics/undraw_investment_xv9d.svg") "Code for the only constant." $ do
    el "p" $ text "The only constants are time and change, what if your code could account for them? Functional reactive programming lets you write code that understands real world dynamics as naturally as you do, but isn’t any harder to write."
  article "left" (static @"img/graphics/undraw_to_the_moon_v1mv.svg") "Batteries included." $ do
    --TODO: Link to Obelisk
    el "p" $ text "Obelisk is the full-stack framework for building Reflex apps using the entire ecosystem. Designed to work ‘out of the box’, with best practices and starter templates included. This is the best option for developing and deploying serious real world applications, quickly."

  {-
home :: (DomBuilder t m, RouteToUrl (R Route) m, SetRoute t (R Route) m) => m ()
home = do

  el "main" $ do

    el "h1" $ text "Reflex adapts to changes in your..."

    elClass "section" "cards" $ do
      elClass "article" "card" $ do
        el "h3" $ do
          -- TODO icon
          text "Data"
        text "Reflex Apps automatically update in response to change. This keeps every interaction live, accurately representing the relationship your data has with the real world."
        text "Reflex is the key to writing self- updating user interfaces."
      elClass "article" "card" $ do
        el "h3" $ do
          -- TODO icon
          text "Requirements"
        text "Reflex components are modular and reusable. If the requirements for an App change it can quickly and easily be retooled. The alterations are pushed through the entire system quickly."
        text "The modularity of Reflex Apps keeps your development on time."
      elClass "article" "card" $ do
        el "h3" $ do
          -- TODO icon
          text "Platform"
        text "Reflex has been built to seamlessly support interfaces on desktop, mobile, web and many other platforms. Regardless of how your platform needs change, your Reflex App won’t require adaptation."
        text "Write code once, run it nearly anywhere."

    elClass "hr" "short" blank

    el "h1" $ text "The Ecosystem Built For Evolution."


    elClass "section" "alternating" $ do
      let article side url = elAttr "article" ("class" =: ("alternating " <> side) <> "style" =: "background-image: url(" <> url <> ")")
      article "left" (static @"img/graphics/undraw_speed_test_wxl0.svg") $ do
        el "h3" $ text "Build fast without breaking things."
        el "p" $ text "You don’t have to choose between building quickly or sustainably anymore. ReflexFRP allows you to write production quality code from the get-go, with less technical debt."

      article "right" (static @"img/graphics/undraw_code_typing_7jnv.svg") $ do
        el "h3" $ text "Never lost in translation."
        el "p" $ text "ReflexPlatform allows you to write entirely in Haskell, everywhere. Using one language does away with client / server protocol mismatches, and separate engineering teams."

      article "left" ("src" =: static @"img/graphics/undraw_good_team_m7uu.svg") $ do
        el "h3" $ text "Jump the complexity wall."
        el "p" $ text "As you add to your codebase, it inevitably becomes more complex. Reflex Apps are organized and ordered so that their complexity grows in line with the size of the program. Build big Apps with basic complications."

      article "right" ("src" =: static @"img/graphics/undraw_investment_xv9d.svg") $ do
        el "h3" $ text "Code for the only constant."
        el "p" $ text "Your mind can account for occurrences like time and change, what if your code could too? Functional reactive programming lets you say what you mean, instead of trying to explaining what you mean in several smaller segments."

      article "left" ("src" =: static @"img/graphics/undraw_to_the_moon_v1mv.svg") $ do
        el "h3" $ text "Batteries included."
        el "p" $ text "Ready to start creating with Reflex? Then it’s time to download Obelisk. Designed to work ‘out of the box’ with best practices included. This is the best option for developing and deploying serious real world applications, quickly."

    b <- button "Checkout the Get Started guide"
    pure ()

  divClass "footer" $ do
    text "Reflex"
    text "Twitter"
    text "Medium"
    -}
