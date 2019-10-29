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
import Frontend.CommonWidgets
import qualified NeatInterpolation as NI

tutorial :: DomBuilder t m => Section m
tutorial = Section
  { _section_title = "Tutorial"
  , _section_content = do
    elClass "p" "description" $ text "This tutorial assumes a functional familiarity with Haskell."
  , _section_subsections = [getStarted, settingUpNix]
  }

getStarted :: DomBuilder t m => Section m
getStarted = Section
  { _section_title = "Before We Get Started"
  , _section_content = do
    elClass "p" "description" $ text "This tutorial will walk you through the installation of Nix and Obelisk, and then set you up with the Reflex library."
    el "p" $ text "Once Nix is downloaded and we’re running Obelisk, we will build a simple functional reactive calculator that can be used in a web browser. Even if you don’t plan to build a calculator for your own project, this tutorial is helpful as it teaches the fundamentals of building a Reflex application. A familiarity with this process will make developing other applications much easier."
    el "p" $ text "If you already have Nix and Obelisk up and running, feel free to skip down to the Overview, or even straight to the tutorial if you're itching to write some code."
  , _section_subsections = [theBuild]
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
    el "ol" $ do
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
          inlineSnippet "/etc/nixos/configuration.nix"
        snippet "nix line-numbers" [NI.text|
          x.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
          nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
        |]
        el "p" $ text "and rebuild your NixOS configuration:"
        snippet "bash" "sudo nixos-rebuild switch"
        el "p" $ do
          text "If you are using another operating system or linux distribution, ensure that these lines are present in your Nix configuration file ("
          inlineSnippet "/etc/nix/nix.conf"
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
