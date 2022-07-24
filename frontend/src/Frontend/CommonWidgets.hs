{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Frontend.CommonWidgets where

import Control.Lens hiding (element)
import Control.Monad (void)
import Control.Monad
import Data.Foldable (for_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM as DOM
import Language.Javascript.JSaddle
import Obelisk.Frontend.GoogleAnalytics
import Obelisk.Generated.Static
import Obelisk.Route.Frontend
import Reflex.Dom

extLink :: forall t m a. (Analytics t m, DomBuilder t m) => Text -> m a -> m a
extLink = extLinkAttr mempty

extLinkAttr :: forall t m a. (Analytics t m, DomBuilder t m) => Map Text Text -> Text -> m a -> m a
extLinkAttr attrs href m = do
  (e,a) <- elAttr' "a" ("href" =: href <> attrs <> "target" =: "_blank" <> "rel" =: "noopener") m
  tellAnalytics (gaOutboundClickEvent href <$ (domEvent Click e :: Event t ()))
  return a


-- | TODO this is for marking up unfinished parts in an obvious fashion. It
-- should be removed
unfinished :: DomBuilder t m => Text -> m a -> m a
unfinished t = elAttr "span" ("style" =: "background: red" <> "title" =: t)

reflexLogo :: DomBuilder t m => m ()
reflexLogo = elAttr "img" ("class" =: "reflex-logo" <> "src" =: static @"img/logo.svg" <> "alt" =: "Reflex") blank

routeFragment
  :: forall t m r a . (DomBuilder t m, RouteToUrl r m, Analytics t m)
  => r -> Text -> m a -> m a
routeFragment r frag w = do
  enc <- askRouteToUrl
  let href = enc r <> "#" <> frag
  (e, a) <- elAttr' "a" ("href" =: href) w
  tellAnalytics (gaClickEvent "engagement" href <$ (domEvent Click e :: Event t ()))
  return a

tableOfContents :: (DomBuilder t m, RouteToUrl r m, Analytics t m) => r -> Section m -> m ()
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
  :: (DomBuilder t m, RouteToUrl r m, Analytics t m)
  => r -> Section m -> m ()
sectionPage r mainSection = el "main" $ do
  header 1 $ _section_title mainSection
  _section_content mainSection
  el "hr" blank
  divClass "toc" $ do
    el "aside" $ el "nav" $ tableOfContents r mainSection
    el "article" $ goSections 2 $ _section_subsections mainSection
  where
    goSections prec sections = for_ sections $ \section -> do
      header prec $ _section_title section
      _section_content section
      goSections (prec + 1) $ _section_subsections section
    header (i :: Int) t = elAttr h ("id" =: titleFragment t) $ text t
      where h = "h" <> T.pack (show $ min 6 $ max 1 i)

demoSnippet :: DomBuilder t m => m a -> Text -> m ()
demoSnippet m code = do
  snippet "haskell line-numbers" code
  void m

-- | Code snippet
snippet :: DomBuilder t m => Text -> Text -> m ()
snippet lang = el "pre" . inlineSnippet lang

-- | Inline code snippet
inlineSnippet :: DomBuilder t m => Text -> Text -> m ()
inlineSnippet lang = elClass "code" languageClass . text
  where languageClass = "language-" <> lang

-- | Inline haskell snippet. Used a lot, hence the short name.
hs :: DomBuilder t m => Text -> m ()
hs = inlineSnippet "haskell"

-- | Generic inline monospace text
monospace :: DomBuilder t m => Text -> m ()
monospace = elClass "span" "monospace" . text

-- * Route link: delete this section when obelisk has been updated
--
-- | A link widget that, when clicked, sets the route to the provided route. In non-javascript
-- contexts, this widget falls back to using @href@s to control navigation
routeLink
  :: forall t m a route js.
     ( DomBuilder t m
     , RouteToUrl route m
     , SetRoute t route m
     , Prerender js t m
     )
  => route -- ^ Target route
  -> m a -- ^ Child widget
  -> m a
routeLink r w = do
  (e, a) <- routeLinkImpl mempty r w
  scrollToTop e
  return a

-- | Like 'routeLink', but takes additional attributes as argument.
--
routeLinkAttr
  :: forall t m a route js.
     ( DomBuilder t m
     , RouteToUrl route m
     , SetRoute t route m
     , Prerender js t m
     )
  => Map AttributeName Text -- ^ Additional attributes
  -> route -- ^ Target route
  -> m a -- ^ Child widget
  -> m a
routeLinkAttr attrs r w = do
  (e, a) <- routeLinkImpl attrs r w
  let
    targetBlank = Map.lookup "target" attrs == Just "_blank"
  when (not targetBlank) $ scrollToTop e
  return a

-- | Raw implementation of 'routeLink'. Does not scroll to the top of the page on clicks.
routeLinkImpl
  :: forall t m a route.
     ( DomBuilder t m
     , RouteToUrl route m
     , SetRoute t route m
     )
  => Map AttributeName Text
  -> route -- ^ Target route
  -> m a -- ^ Child widget
  -> m (Event t (), a)
routeLinkImpl attrs r w = do
  enc <- askRouteToUrl
  let
    -- If targetBlank == True, the link will be opened in another page. In that
    -- case, we don't prevent the default behaviour, and we don't need to
    -- setRoute.
    targetBlank = Map.lookup "target" attrs == Just "_blank"
    cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_initialAttributes .~ ("href" =: enc r <> attrs)
        & (if targetBlank
           then id
           else elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (const preventDefault))
  (e, a) <- element "a" cfg w
  when (not targetBlank) $ setRoute $ r <$ domEvent Click e
  return (domEvent Click e, a)

scrollToTop :: forall m t js. (Prerender js t m, Monad m) => Event t () -> m ()
scrollToTop e = prerender_ blank $ performEvent_ $ ffor e $ \_ -> liftJSM $ DOM.currentWindow >>= \case
  Nothing -> pure ()
  Just win -> Window.scrollTo win 0 0
