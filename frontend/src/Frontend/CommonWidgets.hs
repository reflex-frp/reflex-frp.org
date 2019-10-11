{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.CommonWidgets where

import Data.Foldable (for_)
import Data.Text (Text)
import Obelisk.Route.Frontend
import Reflex.Dom
import qualified Data.Text as T

extLink :: DomBuilder t m => Text -> m a -> m a
extLink href m =
  elAttr "a" ("href" =: href <> "target" =: "_blank" <> "rel" =: "noopener") $ m

-- | TODO this is for marking up unfinished parts in an obvious fashion. It
-- should be removed
unfinished :: DomBuilder t m => Text -> m a -> m a
unfinished t = elAttr "span" ("style" =: "background: red" <> "title" =: t)

routeFragment
  :: (DomBuilder t m, RouteToUrl r m)
  => r -> Text -> m a -> m a
routeFragment r frag w = do
  enc <- askRouteToUrl
  elAttr "a" ("href" =: (enc r <> "#" <> frag)) w

tableOfContents :: (DomBuilder t m, RouteToUrl r m) => r -> Section m -> m ()
tableOfContents r topSection = el "ul" $ do
  el "li" $ titleLink $ _section_title topSection
  go $ _section_subsections topSection
  where
    titleLink t = routeFragment r (titleFragment t) (text t)
    go sections = for_ sections $ \section -> el "li" $ do
      titleLink $ _section_title section
      el "ul" $ go $ _section_subsections section

titleFragment :: Text -> Text
titleFragment = T.toLower . T.intercalate "-" . T.words

data Section m = Section
  { _section_title :: Text
  , _section_content :: m ()
  , _section_subsections :: [Section m]
  }

sectionPage
  :: (DomBuilder t m, RouteToUrl r m)
  => r -> Section m -> m ()
sectionPage r mainSection = do
  header 1 $ _section_title mainSection
  _section_content mainSection
  el "hr" blank
  divClass "toc" $ do
    el "nav" $ tableOfContents r mainSection
    el "article" $ goSections 2 $ _section_subsections mainSection
  where
    goSections prec sections = for_ sections $ \section -> do
      header prec $ _section_title section
      _section_content section
      goSections (prec + 1) $ _section_subsections section
    header (i :: Int) t = elAttr h ("id" =: titleFragment t) $ text t
      where h = "h" <> T.pack (show $ min 6 $ max 1 i)
