{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Frontend.Page.Tutorial (tutorial) where

import Reflex.Dom
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import qualified NeatInterpolation as NI

import Common.Route
import Frontend.CommonWidgets

tutorial :: (DomBuilder t m, RouteToUrl (R Route) m, SetRoute t (R Route) m, Prerender js t m) => Section m
tutorial = Section
  { _section_title = "Tutorial"
  , _section_content = do
    elClass "p" "description" $ text "This tutorial assumes a functional familiarity with Haskell."
  , _section_subsections = [getStarted, obTutorial, summary]
  }

getStarted :: DomBuilder t m => Section m
getStarted = Section
  { _section_title = "Before We Get Started"
  , _section_content = do
    elClass "p" "description" $ text "This tutorial will walk you through the installation of Nix and Obelisk, and then set you up with the Reflex library."
    el "p" $ text "Once Nix is downloaded and we’re running Obelisk, we will build a simple functional reactive calculator that can be used in a web browser. Even if you don’t plan to build a calculator for your own project, this tutorial is helpful as it teaches the fundamentals of building a Reflex application. A familiarity with this process will make developing other applications much easier."
    el "p" $ text "If you already have Nix and Obelisk up and running, feel free to skip down to the Overview, or even straight to the tutorial if you're itching to write some code."
  , _section_subsections = [theBuild, settingUpNix, obelisk]
  }

theBuild :: DomBuilder t m => Section m
theBuild = Section
  { _section_title = "The Build"
  , _section_content = do
    el "p" $ do
      text "In this tutorial, we’ll walk through the steps of building a functional reactive calculator with Reflex for use in a web browser.  You can see what that will look like "
      unfinished "link" $ text "here"
      text ". If you scroll down and don’t see any code that makes sense to you, don’t worry, it will! The goal of this tutorial is to guide you through the basics of functional reactive programming and the Reflex syntax, all will be explained as we go along."
    el "p" $ do
      text "This tutorial does assume prior knowledge of Haskell, if you’re new to the language and would like some review, check out this "
      extLink "https://www.schoolofhaskell.com/user/bartosz/basics-of-haskell" $ text "12 part educational series"
      text ". It’s a gentle introduction to Haskell aimed at those with an imperative background and it’ll get you up to speed quickly."
    el "p" $ text "Without further ado, let’s get started."
  , _section_subsections = []
  }

settingUpNix :: DomBuilder t m => Section m
settingUpNix = Section
  { _section_title = "Setting Up Nix"
  , _section_content = do
    el "p" $ text "Nix is a package manager for Linux and other Unix systems that provides easy setup of build environments and multi-user package management."
    el "p" $ text "We use Nix to reliably build and package Obelisk, follow through the next three steps to install Nix and set up its caches."
    elClass "ol" "big-numbers" $ do
      el "li" $ do
        el "p" $ do
          text "Get started "
          extLink "https://nixos.org/nix/" $ text "installing Nix"
          text ". If you already have Nix installed, make sure you have version 2.0 or higher."
        el "p" $ text "To check your current version of Nix, run:"
        snippet "nix" "nix-env --version"
      el "li" $ do
        el "p" $ text "Next, set up nix caches"
        el "p" $ text "This can speed up the build by downloading packages from caches instead of compiling everything yourself."
        el "p" $ do
          text "If you are running NixOS, add this to "
          monospace "/etc/nixos/configuration.nix"
        snippet "nix line-numbers" [NI.text|
          x.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
          nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
        |]
        el "p" $ text "and rebuild your NixOS configuration:"
        snippet "bash" "sudo nixos-rebuild switch"
        el "p" $ do
          text "If you are using another operating system or linux distribution, ensure that these lines are present in your Nix configuration file ("
          monospace "/etc/nix/nix.conf"
          text " on most systems; see full list "
          extLink "https://nixos.org/nix/manual/#sec-conf-file" $ text "here"
          text ")."
        snippet "nix line-numbers" [NI.text|
          substituters = https://cache.nixos.org https://nixcache.reflex-frp.org
          trusted-public-keys =
              cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
              ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=
        |]
        el "p" $ do
          text "If you use a different Linux, also enable sandboxing (see "
          extLink "https://github.com/obsidiansystems/obelisk/issues/172" $ text "issue #172"
          text " or "
          extLink "https://github.com/obsidiansystems/obelisk/issues/6" $ text "issue #6"
          text " if you run into build problems)."
        snippet "nix" "sandbox = true"
        el "p" $ text "If you are using MacOS, disable sandboxing (there are still some impure dependencies for now)."
        snippet "nix" "sandbox = false"
        el "p" $ text "Then restart the nix daemon."
        snippet "bash" [NI.text|
          sudo launchctl stop org.nixos.nix-daemon
          sudo launchctl start org.nixos.nix-daemon
        |]
      el "li" $ do
        el "p" $ text "Now install Obelisk:"
        snippet "bash" "nix-env -f https://github.com/obsidiansystems/obelisk/archive/master.tar.gz -iA command"
  , _section_subsections = []
  }

obelisk :: DomBuilder t m => Section m
obelisk = Section
  { _section_title = "Developing in Obelisk"
  , _section_content = do
    el "p" $ text "To create a new Obelisk project, go to an empty directory and run:"
    snippet "bash" "ob init"
    el "p" $ text "Obelisk leverages ghcid to provide a live-reloading server that handles both the frontend and backend. To run your Obelisk app and monitor the source for changes, run:"
    snippet "bash" "ob run"
    unfinished "TODO" $ text "*bridge* -not me"
  , _section_subsections = []
  }

obTutorial :: DomBuilder t m => Section m
obTutorial = Section
  { _section_title = "Tutorial"
  , _section_content = blank
  , _section_subsections = [basicsOfDOM, dynamicsAndEvents, numberInput, adding, multipleOps, dynAttrs]
  }

basicsOfDOM :: DomBuilder t m => Section m
basicsOfDOM = Section
  { _section_title = "The Basics of DOM"
  , _section_content = do
    el "p" $ text "Reflex’s companion library (Reflex DOM), contains a number of functions used to build and interact with the Document Object Model. Let’s start by getting a basic app up and running."
    snippet "haskell line-numbers" [NI.text|
      {-# LANGUAGE OverloadedStrings #-}
      import Reflex.Dom
      main = mainWidget $ el "div" $ text "Welcome to Reflex"
    |]
    el "p" $ unfinished "convert to obelisk" $ do
      text "Save this file as "
      monospace "source.hs"
      text " and compile it by running "
      monospace "ghcjs source.hs"
      text ". If you’ve entered everything correctly, this will produce a folder named "
      monospace "source.jsexe"
      text " in the same directory as "
      monospace "source.hs"
      text "."
    el "p" $ do
      text "Navigate to this folder in your file manager and open "
      monospace "index.html"
      text " using your browser. The browser should show a page with the text “Welcome to Reflex”."
    el "p" $ do
      text "Most Reflex apps will start the same way: a call to "
      hs "mainWidget"
      text " with a starting widget"
      text "."
    el "p" $ do
      text "A widget is some DOM wrapped up for easy use with Reflex. In this tutorial, we are building the argument to "
      hs "mainWidget"
      text " (AKA, our starting Widget), on the same line."
    el "p" $ do
      hs "el"
      text " has the type signature:"
    snippet "haskell" [NI.text|
      el :: DomBuilder t m => Text -> m a -> m a
    |]
    el "p" $ do
      text "The first argument to "
      hs "el"
      text " is "
      hs "Text"
      text ", which will become the tag of the html element produced. The second argument is a widget, which will become the child of the element being produced."
    el "p" $ do
      text "We turned on the "
      hs "OverloadedStrings"
      text " extension so that the literal string in our source file would be interpreted as the appropriate type ("
      hs "Text"
      text " rather than "
      hs "String"
      text ")."
    elClass "section" "article-note" $ do
      el "h4" $ text "Note: Interpreting the DomBuilder type"
      el "p" $ do
        text "FRP-enabled datatypes in Reflex take an argument "
        hs "t"
        text ", which identifies the FRP subsystem being used. This ensures that wires don't get crossed if a single program uses Reflex in multiple different contexts."
      el "p" $ do
        text "You can think of "
        hs "t"
        text " as identifying a particular \"timeline\" of the FRP system. Because most simple programs will only deal with a single timeline, we won't revisit the "
        hs "t"
        text " parameters in this tutorial."
      el "p" $ do
        text "As long as you make sure your "
        hs "Event"
        text ", "
        hs "Behavior"
        text ", and "
        hs "Dynamic"
        text " values all get their "
        hs "t"
        text " argument, it'll work itself out."
    el "p" $ do
      text "In our example, "
      hs [NI.text|el "div" $ text "Welcome to Reflex"|]
      text ", the first argument to "
      hs "el"
      text " was "
      hs [NI.text|"div"|]
      text ", indicating that we are going to produce a div element."
    el "p" $ do
      text "The second argument to "
      hs "el"
      text " was "
      hs [NI.text|text "Welcome to Reflex"|]
      text ". The type signature of "
      hs "Text"
      text " is:"
    snippet "haskell" [NI.text|
      text :: DomBuilder t m => Text -> m ()
    |]
    el "p" $ do
      hs "text"
      text " takes a "
      hs "Text"
      text " and produces a widget. It becomes a text DOM node in the parent element."
    el "p" $ do
      text "Of course, instead of a "
      hs "Text"
      text ", we could have used "
      hs "el"
      text " here as well to continue building arbitrarily complex DOM. For instance, if we wanted to make an unordered list:"
    snippet "haskell line-numbers" [NI.text|
      {-# LANGUAGE OverloadedStrings #-}
      import Reflex.Dom

      main = mainWidget $ el "div" $ do
        el "p" $ text "Reflex is:"
        el "ul" $ do
          el "li" $ text "Efficient"
          el "li" $ text "Higher-order"
          el "li" $ text "Glitch-free"
    |]
  , _section_subsections = []
  }

dynamicsAndEvents :: DomBuilder t m => Section m
dynamicsAndEvents = Section
  { _section_title = "Dynamics and Events"
  , _section_content = do
    el "p" $ text "Of course, we want to do more than just view a static webpage. Let’s start by getting some user input and then printing it."
    snippet "haskell line-numbers" [NI.text|
      {-# LANGUAGE OverloadedStrings #-}
      import Reflex.Dom
      main = mainWidget $ el "div" $ do
        t <- inputElement def
        dynText $ _inputElement_value t
    |]
    el "p" $ do
      text "Running this in your browser, you’ll see that it produces a "
      monospace "div"
      text " containing an "
      monospace "input"
      text " element. When you type into the "
      monospace "input"
      text " element, the text you enter appears inside the "
      monospace "div"
      text " as well."
    el "p" $ do
      hs "InputElement"
      text " is a function with the following type:"
    snippet "haskell" [NI.text|
      inputElement :: DomBuilder t m
        => InputElementConfig er t (DomBuilderSpace m)
        -> m (InputElement er (DomBuilderSpace m) t)
    |]
    el "p" $ do
      text "It takes an "
      hs "InputElementConfig"
      text " (given a default value in our example), and produces a widget whose result is an "
      hs "InputElement"
      text ". The "
      hs "InputElement"
      text " exposes the following functionality:"
    snippet "haskell" [NI.text|
      data InputElement er d t = InputElement
        { _inputElement_value :: Dynamic t Text
        , _inputElement_checked :: Dynamic t Bool
        , _inputElement_checkedChange :: Event t Bool
        , _inputElement_input :: Event t Text
        , _inputElement_hasFocus :: Dynamic t Bool
        , _inputElement_element :: Element er d t
        , _inputElement_raw :: RawInputElement d
        , _inputElement_files :: Dynamic t [File]
        }
    |]
    el "p" $ do
      text "Here we are using "
      hs "_inputElement_value"
      text " to access the "
      hs "Dynamic Text"
      text " value of the "
      hs "InputElement"
      text "."
    el "p" $ do
      text "Conveniently, "
      hs "dynText"
      text " takes a "
      hs "Dynamic Text"
      text " and displays it. It is the dynamic version of "
      hs "text"
      text "."
  , _section_subsections = []
  }

numberInput :: DomBuilder t m => Section m
numberInput = Section
  { _section_title = "A Number Input"
  , _section_content = do
    el "p" $ text "A calculator was promised, we know. We'll start building the calculator now by creating an input for numbers."
    snippet "haskell line-numbers" [NI.text|
      {-# LANGUAGE OverloadedStrings #-}
      import Reflex
      import Reflex.Dom
      import Data.Map (Map)
      import qualified Data.Map as Map

      main = mainWidget $ el "div" $ do
        t <- inputElement $ def
          & inputElementConfig_initialValue .~ "0"
          & inputElementConfig_elementConfig .  elementConfig_initialAttributes .~ ("type" =: "number")
        dynText $ _inputElement_value t
    |]
    el "p" $ do
      text "The code above overrides some of the default values of the "
      hs "InputElementConfig"
      text ". We provide a "
      hs "Map Text Text"
      text " value for the "
      hs "inputElementConfig_elementConfig"
      text "'s "
      hs "elementConfig_initialAttributes"
      text ", Specifying the html input element’s "
      monospace "type"
      text " attribute to "
      monospace "number"
      text "."
    el "p" $ do
      text "Next, we override the default initial value of the "
      hs "InputElement"
      text ". We gave it "
      hs [NI.text|“0”|]
      text ". Even though we’re making an html input element with the attribute "
      monospace "type=number"
      text ", the result is still a "
      hs "Text"
      text ". We’ll covert this later."
    el "p" $ text "Let’s do more than just take the input value and print it out. First, let’s make sure the input is actually a number:"
    el "p" $ do
      text "We’ve defined a function "
      hs "numberInput"
      text " that both handles the creation of the "
      hs "InputElement"
      text " and reads its value. Recall that "
      hs "_inputElement_value"
      text " gives us a "
      hs "Dynamic Text"
      text ". The final line of code in "
      hs "numberInput"
      text " uses "
      hs "fmap"
      text " to apply the function "
      hs "readMaybe . unpack"
      text " to the "
      hs "Dynamic"
      text " value of the "
      hs "InputElement"
      text ". This produces a "
      hs " Dynamic (Maybe Double)"
      text "."
    el "p" $ do
      text "Our "
      hs "main"
      text " function uses "
      hs "fmap"
      text " to map over the "
      hs "Dynamic (Maybe Double)"
      text " produced by "
      hs "numberInput"
      text " and "
      hs "pack . show"
      text " the value it contains. We store the new "
      hs "Dynamic Text"
      text " in "
      hs "numberString"
      text " and feed that into "
      hs "dynText"
      text " to actually display the "
      hs "Text"
      text "."
    snippet "haskell line-numbers" [NI.text|
      {-# LANGUAGE OverloadedStrings #-}
      import Reflex.Dom
      import Data.Map (Map)
      import qualified Data.Map as Map
      import Data.Text (pack, unpack)
      import Text.Read (readMaybe)

      main = mainWidget $ el "div" $ do
        x <- numberInput
        let numberString = fmap (pack . show) x
        dynText numberString

      numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
      numberInput = do
        n <- inputElement $ def
          & inputElementConfig_initialValue .~ "0"
          & inputElementConfig_elementConfig .  elementConfig_initialAttributes .~ ("type" =: "number")
        return . fmap (readMaybe . unpack) $ _inputElement_value n
    |]
    el "p" $ do
      text "Running the app at this point should produce an input and some text showing the "
      hs "Maybe Double"
      text ". Typing in a number should produce output like "
      hs "Just 12.0"
      text " and typing in other text should produce the output "
      hs "Nothing"
      text "."
  , _section_subsections = []
  }

adding :: DomBuilder t m => Section m
adding = Section
  { _section_title = "Adding"
  , _section_content = do
    el "p" $ do
      text "Now that we have "
      hs "numberInput"
      text " we can put together a couple of inputs to make a basic calculator."
    snippet "haskell line-numbers" [NI.text|
      {-# LANGUAGE OverloadedStrings #-}
      import Reflex.Dom
      import Data.Map (Map)
      import qualified Data.Map as Map
      import Data.Text (pack, unpack)
      import Text.Read (readMaybe)

      main = mainWidget $ el "div" $ do
        nx <- numberInput
        text " + "
        ny <- numberInput
        text " = "
        let result = zipDynWith (\x y -> (+) <$> x <*> y) nx ny
            resultString = fmap (pack . show) result
        dynText resultString

      numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
      numberInput = do
        n <- inputElement $ def
          & inputElementConfig_initialValue .~ "0"
          & inputElementConfig_elementConfig .  elementConfig_initialAttributes .~ ("type" =: "number")
        return . fmap (readMaybe . unpack) $ _inputElement_value n
    |]
    el "p" $ do
      hs "numberInput"
      text " hasn’t changed here. Our "
      hs "main"
      text " function now creates two inputs. "
      hs "zipDynWith"
      text " is used to produce the actual sum of the values of the inputs. The type signature of "
      hs "zipDynWith"
      text " is:"
    snippet "haskell" [NI.text|
      Reflex t => (a -> b -> c) -> Dynamic t a -> Dynamic t b -> Dynamic t c
    |]
    el "p" $ do
      text "You can see that it takes a function that combines two pure values and produces some other pure value, and two "
      hs "Dynamic"
      text "’s, and produces a "
      hs "Dynamic"
      text "."
    el "p" $ do
      text "In our case, "
      hs "zipDynWith"
      text " is combining the results of our two "
      hs "numberInput"
      text "s (with a little help from "
      hs "Control.Applicative"
      text ") into a sum."
    el "p" $ do
      text "We use "
      hs "fmap"
      text " again to apply "
      hs "pack . show"
      text " to "
      hs "result :: Dynamic (Maybe Double)"
      text " resulting in a "
      hs "Dynamic Text"
      text ". This "
      hs "resultText"
      text " is then displayed using "
      hs "dynText"
      text "."
  , _section_subsections = []
  }

multipleOps :: DomBuilder t m => Section m
multipleOps = Section
  { _section_title = "Supporting Multiple Operations"
  , _section_content = do
    el "p" $ do
      text "Next, we’ll add support for other operations. We’re going to add a dropdown so that the user can select the operation to apply. The function "
      hs "dropdown"
      text " has the type: "
    snippet "haskell" [NI.text|
      dropdown :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, Ord k)
        => k -> Dynamic t (Map k Text) -> DropdownConfig t k -> m (Dropdown t k)
    |]
    el "p" $ do
      text "The first argument is the initial value of the "
      hs "Dropdown"
      text ". The second argument is a "
      hs "Dynamic (Map k Text)"
      text " that represents the options in the dropdown. The "
      hs "Text"
      text " values of the "
      hs "Map"
      text " are the strings that will be displayed to the user. If the initial key is not in the "
      hs "Map"
      text ", it is added and given a "
      hs "Text"
      text " value of "
      hs [NI.text|""|]
      text ". The final argument is a "
      hs "DropdownConfig"
      text "."
    el "p" $ text "Our supported operations will be:"
    snippet "haskell" [NI.text|
      data Op = Plus | Minus | Times | Divide deriving (Eq, Ord)
      ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]
    |]
    el "p" $ do
      text "We'll use this as an argument to "
      hs "dropdown"
      text ":"
    snippet "haskell" [NI.text|
      d <- dropdown Times (constDyn ops) def
    |]
    el "p" $ do
      text "We are using "
      hs "constDyn"
      text " again here to turn our "
      hs "Map"
      text " of operations into a "
      hs "Dynamic"
      text ". Using "
      hs "def"
      text ", we provide the default "
      hs "DropdownConfig"
      text ". The result, "
      hs "d"
      text ", will be a "
      hs "Dropdown"
      text ". We can retrieve the"
      hs "Dynamic"
      text " selection of a "
      hs "Dropdown"
      text " by using "
      hs "_dropdown_value"
      text "."
    snippet "haskell line-numbers" [NI.text|
      {-# LANGUAGE OverloadedStrings #-}
      import Reflex
      import Reflex.Dom
      import Data.Map (Map)
      import qualified Data.Map as Map
      import Data.Text (pack, unpack, Text)
      import Text.Read (readMaybe)

      main = mainWidget $ el "div" $ do
        nx <- numberInput
        d <- dropdown Times (constDyn ops) def
        ny <- numberInput
        let values = zipDynWith (,) nx ny
            result = zipDynWith (\o (x,y) -> runOp o <$> x <*> y) (_dropdown_value d) values
            resultText = fmap (pack . show) result
        text " = "
        dynText resultText

      numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
      numberInput = do
        n <- inputElement $ def
          & inputElementConfig_initialValue .~ "0"
          & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
        return . fmap (readMaybe . unpack) $ _inputElement_value n

      data Op = Plus | Minus | Times | Divide deriving (Eq, Ord)

      ops :: Map Op Text
      ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]

      runOp :: Fractional a => Op -> a -> a -> a
      runOp s = case s of
        Plus -> (+)
        Minus -> (-)
        Times -> (*)
        Divide -> (/)
    |]
    el "p" $ do
      text "This is our complete program. We've added an uninteresting function "
      hs "runOp"
      text " that takes an "
      hs "Op"
      text " and returns an operation. The keys of the "
      hs "Map"
      text " we used to create the "
      hs "Dropdown"
      text " had the type "
      hs "Op"
      text "."
    el "p" $ do
      text "When we retrieve the value of "
      hs "Dropdown"
      text ", we'll use "
      hs "runOp"
      text " to turn the "
      hs "Dropdown"
      text " selection into the function we need to apply to our numbers."
    el "p" $ do
      text "After creating the two "
      hs "numberInputs"
      text ", we combine them using "
      hs "zipDynWith"
      text " applying "
      hs "(,)"
      text ", making a tuple of type "
      hs "Dynamic (Maybe Double, Maybe Double)"
      text " and binding it to "
      hs "values"
      text "."
    el "p" $ do
      text "Next, we call "
      hs "zipDynWith"
      text " again, combining the "
      hs "_dropdown_value"
      text " and "
      hs "values"
      text ". Now, instead of applying "
      hs "(+)"
      text " to our "
      hs "Double"
      text " values, we use "
      hs "runOp"
      text " to select an operation based on the "
      hs "Dynamic"
      text " value of our "
      hs "Dropdown"
      text "."
    el "p" $ text "Running the app at this point will give us our two number inputs with a dropdown of operations sandwiched between them. Multiplication should be pre-selected when the page loads."
  , _section_subsections = []
  }

dynAttrs :: DomBuilder t m => Section m
dynAttrs = Section
  { _section_title = "Dynamic Element Attribute"
  , _section_content = do
    el "p" $ text "Let’s spare a thought for the user of our calculator and add a little UI styling. Our number input currently looks like this:"
    snippet "haskell" [NI.text|
      numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
      numberInput = do
        n <- inputElement $ def
          & inputElementConfig_initialValue .~ "0"
          & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
        return . fmap (readMaybe . unpack) $ _inputElement_value n
    |]
    el "p" $ text "Let’s give it some html attributes to work with:"
    snippet "haskell" [NI.text|
      numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
      numberInput = do
        let initAttrs = (("type" =: "number") <> ("style" =: "border-color: blue"))
        n <- inputElement $ def
          & inputElementConfig_initialValue .~ "0"
          & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ initAttrs
        return . fmap (readMaybe . unpack) $ _inputElement_value n
    |]
    el "p" $ do
      text "Here, we’re used a "
      hs "Map Text Text"
      text ". This "
      hs "Map"
      text " represents the html attributes of our inputs."
    el "p" $ do
      text "Static attributes are useful and quite common, but attributes will often need to change. Instead of just making the "
      hs "InputElement"
      text " blue, let’s change it’s colour based on whether the input successfully parses to a "
      hs "Double"
      text ":"
    snippet "haskell line-numbers" [NI.text|
      {-# LANGUAGE RecursiveDo #-}
      ...
      numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
      numberInput = do
        let initAttrs = ("type" =: "number") <> (style False)
            color error = if error then "red" else "green"
            style error = "style" =: ("border-color: " <> color error)
            styleChange :: Maybe Double -> Map AttributeName (Maybe Text)
            styleChange result = case result of
              Just _ -> fmap Just (style False)
              Nothing -> fmap Just (style True)
        rec
          n <- inputElement $ def
            & inputElementConfig_initialValue .~ "0"
            & inputElementConfig_elementConfig .  elementConfig_initialAttributes .~ initAttrs
            & inputElementConfig_elementConfig .  elementConfig_modifyAttributes .~ modAttrEv
          let result = fmap (readMaybe . unpack) $ _inputElement_value n
              modAttrEv = fmap styleChange (updated result)
        return result
    |]
    el "p" $ do
      text "Note that we need to add a language pragma here to enable the "
      hs "RecursiveDo"
      text " language extension. Here the "
      hs "style"
      text " function takes a "
      hs "Bool"
      text " value, whether input is correct or not, and it gives a "
      hs "Map"
      text " of attributes with green or red color respectively. The next function "
      hs "styleChange"
      text " actually produces a "
      hs "Map"
      text "which tells which attribute to change."
    el "p" $ do
      text "If the value of a key in the "
      hs "Map"
      text " is a "
      hs "Just"
      text " value then the attribute is either added or modified. If the value of key is "
      hs "Nothing"
      text ", then that attribute is removed. An "
      hs "Event"
      text " of this "
      hs "Map"
      text " is specified in the "
      hs "elementConfig_modifyAttributes"
      text "."
    el "p" $ do
      text "In the first line of the "
      hs "rec"
      text ", we have supplied this "
      hs "Event"
      text " as argument "
      hs "modAttrEv"
      text ". The "
      hs "Dynamic"
      text " value of the input is bound to "
      hs "result"
      text ". The code for parsing this value has not changed. After we bind"
      hs "result"
      text ", we use "
      hs "fmap"
      text " again to apply a switching function to the updated result "
      hs "Event"
      text "."
    el "p" $ do
      text "The switching function checks whether the value was successfully parsed and gives the corresponding "
      hs "Event"
      text " to modify the attributes."
    el "p" $ text "The complete program now looks like this:"
    snippet "haskell line-numbers" [NI.text|
      {-# LANGUAGE OverloadedStrings #-}
      {-# LANGUAGE RecursiveDo #-}
      import Reflex
      import Reflex.Dom
      import Data.Map (Map)
      import qualified Data.Map as Map
      import Data.Text (pack, unpack, Text)
      import Text.Read (readMaybe)

      main = mainWidget $ el "div" $ do
        nx <- numberInput
        d <- dropdown Times (constDyn ops) def
        ny <- numberInput
        let values = zipDynWith (,) nx ny
            result = zipDynWith (\o (x,y) -> runOp o <$> x <*> y) (_dropdown_value d) values
            resultText = fmap (pack . show) result
        text " = "
        dynText resultText

      numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
      numberInput = do
        let initAttrs = ("type" =: "number") <> (style False)
            color error = if error then "red" else "green"
            style error = "style" =: ("border-color: " <> color error)
            styleChange :: Maybe Double -> Map AttributeName (Maybe Text)
            styleChange result = case result of
              Just _ -> fmap Just (style False)
              Nothing -> fmap Just (style True)
        rec
          n <- inputElement $ def
            & inputElementConfig_initialValue .~ "0"
            & inputElementConfig_elementConfig .  elementConfig_initialAttributes .~ initAttrs
            & inputElementConfig_elementConfig .  elementConfig_modifyAttributes .~ modAttrEv
          let result = fmap (readMaybe . unpack) $ _inputElement_value n
              modAttrEv = fmap styleChange (updated result)
        return result

      data Op = Plus | Minus | Times | Divide deriving (Eq, Ord)

      ops :: Map Op Text
      ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]

      runOp :: Fractional a => Op -> a -> a -> a
      runOp s = case s of
        Plus -> (+)
        Minus -> (-)
        Times -> (*)
        Divide -> (/)
    |]
    el "p" $ text "The input border colors will now change depending on their value."
  , _section_subsections = []
  }

summary :: (DomBuilder t m, RouteToUrl (R Route) m, SetRoute t (R Route) m, Prerender js t m) => Section m
summary = Section
  { _section_title = "In Summary"
  , _section_content = do
    el "p" $ text "Congratulations! You’ve written a simple functional reactive calculator with Reflex!"
    el "p" $ text "We hope that you now feel confident in your understanding of Reflex, and are ready to begin working on your own project."
    el "p" $ text "If you have additional time or just want to continue practicing your new Reflex skills, here are some ideas for improvements that you could make to the calculator:"
    el "ol" $ do
      el "li" $ el "p" $ unfinished "unfinished" $ text "how to put calculator on your phone"
    el "p" $ do
      text "Again, nice work on making it to the end of this tutorial, if you have any additional questions, check out our "
      routeLinkScrollToTop (Route_Resources :/ ()) $ text "resources page"
      text "."
    el "p" $ text "Happy Coding!"
  , _section_subsections = []
  }
