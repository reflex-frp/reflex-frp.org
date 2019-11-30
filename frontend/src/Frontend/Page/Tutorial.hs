{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Page.Tutorial (tutorial) where

import Control.Arrow ((&&&))
import Control.Monad.Fix
import Obelisk.Route.Frontend
import Reflex.Dom
import qualified Data.Text as T
import qualified Text.MMark.Internal.Type as MMark

import Common.Route
import Frontend.CommonWidgets
import Reflex.MMark.Render
import Tutorial
import Tutorial.Markdown

parsedTutorial
  :: (MonadFix m, PostBuild t m, MonadHold t m, DomBuilder t m)
  => [Either (m ()) MMark.Bni]
parsedTutorial = go =<< parsedMarkdown
  where
    go = \case
      MMark.CodeBlock _ tutorialCode
        -- TODO deduplicate
        | (_:_) <- T.breakOnAll "tutorial1 ::"  tutorialCode -> [Left $ demoSnippet tutorial1  tutorialCode]
        | (_:_) <- T.breakOnAll "tutorial2 ::"  tutorialCode -> [Left $ demoSnippet tutorial2  tutorialCode]
        | (_:_) <- T.breakOnAll "tutorial3 ::"  tutorialCode -> [Left $ demoSnippet tutorial3  tutorialCode]
        | (_:_) <- T.breakOnAll "tutorial4 ::"  tutorialCode -> [Left $ demoSnippet tutorial4  tutorialCode]
        | (_:_) <- T.breakOnAll "tutorial5 ::"  tutorialCode -> [Left $ demoSnippet tutorial5  tutorialCode]
        | (_:_) <- T.breakOnAll "tutorial6 ::"  tutorialCode -> [Left $ demoSnippet tutorial6  tutorialCode]
        | (_:_) <- T.breakOnAll "tutorial7 ::"  tutorialCode -> [Left $ demoSnippet tutorial7  tutorialCode]
        | (_:_) <- T.breakOnAll "tutorial8 ::"  tutorialCode -> [Left $ demoSnippet tutorial8  tutorialCode]
        | (_:_) <- T.breakOnAll "tutorial9 ::"  tutorialCode -> [Left $ demoSnippet tutorial9  tutorialCode]
        | (_:_) <- T.breakOnAll "tutorial10 ::" tutorialCode -> [Left $ demoSnippet tutorial10 tutorialCode]
        | (_:_) <- T.breakOnAll "tutorial11 ::" tutorialCode -> [Left $ demoSnippet tutorial11 tutorialCode]
      MMark.CodeBlock Nothing block -> [Right $ MMark.CodeBlock (Just "haskell") block]
      block -> [Right block]

renderReflex'
  :: DomBuilder t m
  => (x -> m ())
  -> [Either x MMark.Bni]
  -> m ()
renderReflex' f md = mapM_ (either f rBlock) md
  where
    rBlock
      = fix defaultBlockRender
      . fmap rInlines
    rInlines
      = (MMark.mkOisInternal &&& mapM_ (fix defaultInlineRender))

tutorial
  :: ( MonadFix m
     , PostBuild t m
     , MonadHold t m
     , DomBuilder t m
     , RouteToUrl (R Route) m
     , SetRoute t (R Route) m
     , Prerender js t m
     )
  => m ()
tutorial = renderReflex' id parsedTutorial

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
