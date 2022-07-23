{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Frontend.Page.Home (home) where

import Common.Route
import Control.Lens hiding (element)
import Control.Monad
import Data.Text (Text)
import JSDOM.WaitForJS
import Language.Javascript.JSaddle
import Obelisk.Frontend.GoogleAnalytics
import Obelisk.Generated.Static
import Obelisk.Route.Frontend
import Reflex.Dom

import Frontend.CommonWidgets

home :: (DomBuilder t m, RouteToUrl (R Route) m, SetRoute t (R Route) m, Prerender js t m, Analytics t m) => m ()
home = do
  slogan
  elClass "hr" "short top-container" blank
  valueProp
  elClass "hr" "short bottom-container" blank
  benefits
  callToAction "" --TODO: The bottom-of-page call-to-action should probably be different from the top-of-page one

slogan :: forall js t m. (DomBuilder t m, RouteToUrl (R Route) m, SetRoute t (R Route) m, Prerender js t m, Analytics t m) => m ()
slogan = divClass "jumbotron" $ do
  elAttr "div" ("id" =: particlesId) blank
  prerender_ blank $ do
    pb <- getPostBuild
    performEvent_ $ ffor pb $ \_ -> initParticles
  elClass "h1" "tagline" $ text "The world changes," >> el "br" blank >> text "your apps should keep up."
  callToAction ""

particlesId :: Text
particlesId = "particles"

initParticles :: MonadJSM m => m ()
initParticles = withGlobalJS "particlesJS" $ \pjs -> liftJSM $
  void $ pjs ^. js2 ("load" :: Text) particlesId (static @"js/particlesjs-config.json")

callToAction :: forall js t m. (DomBuilder t m, RouteToUrl (R Route) m, SetRoute t (R Route) m, Prerender js t m, Analytics t m) => Text -> m ()
callToAction c = do
  (e, _) <- elAttr' "div" ("class" =: ("call-to-action " <> c)) $
    routeLinkAttr ("role" =: "button") (Route_GetStarted :/ ()) $
      text "Get Started"
  tellAnalytics (gaClickEvent "engagement" "jumbotron" <$ (domEvent Click e :: Event t ()))

valueProp :: DomBuilder t m => m ()
valueProp = do
  elAttr "h1" ("id" =: "learn-more" <> "class" =: "centered close") $
    text "Reflex adapts to changes in your..."
  elClass "section" "cards grid" $ do
    let card icon title child = el "article" $ do
          el "h3" $ elClass "i" icon blank >> text title
          child
    card "icon-stats purple" "Data" $ do
      el "p" $ text "Reflex apps automatically react to changing data. This keeps every interaction current, accurately representing the relationship between your data and the real world."
      el "p" $ text "Reflex is the key to writing self-updating user interfaces."
    card "icon-list purple" "Requirements" $ do
      el "p" $ text "Reflex components are modular and reusable. If your requirements change, your app can quickly and easily be reworked. The modularity of Reflex lets you iterate quickly, without wasting code."
      el "p" $ text "Develop efficiently no matter how many times you pivot."
    card "icon-devices purple" "Platform" $ do
      el "p" $ text "Reflex has been built to seamlessly support interfaces on desktop, mobile, web, and other platforms, all in Haskell. Regardless of your platform needs, Reflex lets you take your team and your code with you."
      el "p" $ text "One team, one code base, every platform."

benefits :: DomBuilder t m => m ()
benefits = do
  elClass "h1" "centered" $ text "The ecosystem built for evolution."
  let article side url title child = elAttr "article" ("class" =: ("alternating " <> side) <> "style" =: ("background-image: url(" <> url <> ")")) $ do
        el "h3" $ text title
        child
  article "left" (static @"img/graphics/undraw_speed_test_wxl0.svg") "Build fast without breaking things." $ do
    elClass "p" "description" $ text "You don’t have to choose between building quickly or sustainably anymore. Reflex-FRP allows you to write production quality code from the get-go, with less technical debt."
  article "right" (static @"img/graphics/undraw_code_typing_7jnv.svg") "Never lost in translation." $ do
    elClass "p" "description" $ text "Reflex platform allows you to write entirely in Haskell, everywhere. Using one language does away with client / server protocol mismatches and separate engineering teams. Code becomes easier to write, with fewer kinds of bugs possible."
  article "left" (static @"img/graphics/undraw_good_team_m7uu.svg") "Jump the complexity wall." $ do
    elClass "p" "description" $ text "As you add to your codebase, it inevitably becomes more complex, but it shouldn’t become harder to manage. No matter the size of your App, Reflex allows for individual pieces of code to be independently understandable."
  article "right" (static @"img/graphics/undraw_investment_xv9d.svg") "Code for the only constant." $ do
    elClass "p" "description" $ text "The only constants are time and change, what if your code could account for them? Functional reactive programming lets you write code that understands real world dynamics as naturally as you do, but isn’t any harder to write."
  article "left" (static @"img/graphics/undraw_to_the_moon_v1mv.svg") "Batteries included." $ do
    elClass "p" "description" $ text "Obelisk is the full-stack framework for building Reflex apps using the entire ecosystem. Designed to work ‘out of the box’, with best practices and starter templates included. This is the best option for developing and deploying serious real world applications, quickly."

