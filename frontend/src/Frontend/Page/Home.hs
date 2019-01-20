{-# LANGUAGE OverloadedStrings #-}
module Frontend.Page.Home (home) where

import Reflex.Dom

home :: DomBuilder t m => m ()
home = divClass "" $ do
  elClass "p" "class" $ do
    text "Reflex is a fully-deterministic, higher-order Functional Reactive Programming (FRP) interface and an engine that efficiently implements that interface."

  divClass "" $ do
    elClass "h4" "class" $ do
      text "Why use Reflex"

    el "ul" $ do
      el "li" $ do
        el "h4" $ text "Full-Stack Haskell"
        el "p" $ text $ "Share data types between frontend and server code; "
          <> "use generics to automatically derive JSON encoding-decoding; "
          <> "reuse code from server at client-side to implement complex business logic, "
          <> "and do prerendering at server-side using frontend code."

        el "p" $ text $ "Fast to prototype; thanks to strong static typing, avoid frustrating runtime errors while doing modifications."
          <> ""

        el "p" $ text $ "Haskell's advanced type-system and compile time checks help create easy to maintain apps."

      el "li" $ do
        el "h4" $ text "Works on Web, Desktop, Android and iOS"
        el "p" $ text $ "Same code can be cross compiled to Web/JavaScript, "
          <> "WebGTK based desktop apps, "
          <> "native Android and iOS apps."

      el "li" $ do
        el "h4" $ text "Composability"
        el "p" $ text $ "Reuse and compose your widgets with confidence, thanks to excellent composability support of FRP architecture."
        el "p" $ text $ "Widgets can handle their internal state, DOM creation, user interaction, and communication with backend. "
          <> "Their type signature clearly shows what inputs they need, and what output they can produce; "
          <> "which greatly simplifies the process of creating complex designs using simpler blocks."
