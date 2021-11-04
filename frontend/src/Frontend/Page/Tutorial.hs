{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Frontend.Page.Tutorial (tutorial) where

import Control.Arrow ((&&&))
import Control.Monad.Fix
import Reflex.Dom
import qualified Data.Text as T
import qualified Text.MMark.Internal.Type as MMark

import Frontend.CommonWidgets
import Reflex.MMark.Render
import Obelisk.Frontend.GoogleAnalytics
import qualified Text.URI as URI
import Tutorial
import Tutorial.Markdown

parsedTutorial
  :: (MonadFix m, PostBuild t m, MonadHold t m, DomBuilder t m, Analytics t m)
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
  :: (DomBuilder t m, Analytics t m)
  => (x -> m ())
  -> [Either x MMark.Bni]
  -> m ()
renderReflex' f md = mapM_ (either f rBlock) md
  where
    rBlock
      = fix defaultBlockRender
      . fmap rInlines
    rInlines
      = MMark.mkOisInternal &&& mapM_ (fix $ defaultInlineRender rLink)
    rLink url mtitle
      = let attrs = maybe mempty ("title" =:) mtitle
         in extLinkAttr attrs (URI.render url)

tutorial
  :: ( MonadFix m
     , PostBuild t m
     , MonadHold t m
     , DomBuilder t m
     , Analytics t m
     )
  => m ()
tutorial = renderReflex' id parsedTutorial
