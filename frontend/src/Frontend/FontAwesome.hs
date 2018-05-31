{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
module Frontend.FontAwesome
  ( faIcon
  , faIcon'
  , Size (..)
  , Pull (..)
  , Animation (..)
  , Rotation (..)
  , Flip (..)
  , FontAwesomeConfig
  , fontAwesomeConfig_size
  , fontAwesomeConfig_fixedWidth
  , fontAwesomeConfig_border
  , fontAwesomeConfig_pull
  , fontAwesomeConfig_animation
  , fontAwesomeConfig_rotation
  , fontAwesomeConfig_flip
  , fontAwesomeConfig_listIcon
  , fontAwesomeCDN
  , dynIconAttr
  , dynIcon2xAttr
  , dynIcon
  , dynIcon2x
  -- Deprecated functions
  , icon
  , iconlg
  , icon2x
  , icon3x
  , icon4x
  , icon5x
  , faLg
  , fa2x
  , fa3x
  , fa4x
  , fa5x
  -- * Re-exports
  , fontAwesomeClass
  , FontAwesome (..)
  ) where

import Data.Map (Map)
import Reflex.Dom.Core
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

import Data.Default
import Data.Maybe

import Web.FontAwesomeType --  FontAwesome Enumerations

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens (makeLenses)
#else
import Control.Lens (Lens')
#endif

--  Control the size of the icon.  See http://fontawesome.io/examples/#larger
data Size
  = Size_Default
  | Size_Large --  Make icon 33% bigger
  | Size_2x    --  Double icon size
  | Size_3x    --  Triple icon size
  | Size_4x    --  Cuadruple icon size
  | Size_5x    --  Quintuple icon size

--  Pull the icon left or right.  See http://fontawesome.io/examples/#bordered-pulled
data Pull
  = Pull_Left  --  Float left
  | Pull_Right --  Float right

--  Animate the icon.  See http://fontawesome.io/examples/#animated
data Animation
  = Animation_Spin
  | Animation_Pulse

--  Rotate the icon.  See http://fontawesome.io/examples/#rotated-flipped
data Rotation = Rotate_90
              | Rotate_180
              | Rotate_270

--  Flip the icon. See http://fontawesome.io/examples/#rotated-flipped
data Flip = Flip_Horizontal
          | Flip_Vertical

--  Optional attributes for FontAwesome icons.  For the default set of options, use 'def'
data FontAwesomeConfig = FontAwesomeConfig
  { _fontAwesomeConfig_size :: Size
  , _fontAwesomeConfig_fixedWidth :: Bool
  , _fontAwesomeConfig_border :: Bool
  , _fontAwesomeConfig_pull :: Maybe Pull
  , _fontAwesomeConfig_animation :: Maybe Animation
  , _fontAwesomeConfig_rotation :: Maybe Rotation
  , _fontAwesomeConfig_flip :: Maybe Flip
  , _fontAwesomeConfig_listIcon :: Bool
  }

instance Default FontAwesomeConfig where
  def = FontAwesomeConfig
    { _fontAwesomeConfig_size = Size_Default
    , _fontAwesomeConfig_fixedWidth = False
    , _fontAwesomeConfig_border = False
    , _fontAwesomeConfig_pull = Nothing
    , _fontAwesomeConfig_animation = Nothing
    , _fontAwesomeConfig_rotation = Nothing
    , _fontAwesomeConfig_flip = Nothing
    , _fontAwesomeConfig_listIcon = False
    }

--  This function takes an FontAwesomeConfig type and generates the necessary
-- "fa" class names for desired icon behavior
fontAwesomeConfigClass :: FontAwesomeConfig -> Text
fontAwesomeConfigClass c = T.intercalate " " . catMaybes $
  [ Just "fa" --TODO: This starting space should go below where fontAwesomeConfigClass is used.  The fact that it's here indicates that fontAwesomeConfigClass knows where it will be used, which it shouldn't.
  , case _fontAwesomeConfig_size c of
      Size_Default -> Nothing
      Size_Large -> Just "fa-lg"
      Size_2x -> Just "fa-2x"
      Size_3x -> Just "fa-3x"
      Size_4x -> Just "fa-4x"
      Size_5x -> Just "fa-5x"
  , if _fontAwesomeConfig_fixedWidth c
    then Just "fa-fw"
    else Nothing
  , if _fontAwesomeConfig_border c
    then Just "fa-border"
    else Nothing
  , case _fontAwesomeConfig_pull c of
      Just Pull_Right -> Just "fa-pull-right"
      Just Pull_Left -> Just "fa-pull-left"
      Nothing -> Nothing
  , case _fontAwesomeConfig_animation c of
      Just Animation_Pulse -> Just "fa-pulse"
      Just Animation_Spin -> Just "fa-spin"
      Nothing -> Nothing
  , case _fontAwesomeConfig_rotation c of
      Just Rotate_90 -> Just "fa-rotate-90"
      Just Rotate_180 -> Just "fa-rotate-180"
      Just Rotate_270 -> Just "fa-rotate-270"
      Nothing -> Nothing
  , case _fontAwesomeConfig_flip c of
      Just Flip_Horizontal -> Just "fa-flip-horizontal"
      Just Flip_Vertical -> Just "fa-flip-vertical"
      Nothing -> Nothing
  , if _fontAwesomeConfig_listIcon c
    then Just "fa-li"
    else Nothing
  ]
--TODO: Parts of this should go upstream

--  Generates a <link> tag that references the MaxCDN bootstrap content
fontAwesomeCDN :: DomBuilder t m => m ()
fontAwesomeCDN = elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css") $ return ()

--TODO: Shouldn't these be replaced with typesafe variants as well?
dynIconAttr :: (DomBuilder t m, PostBuild t m) => Map Text Text -> Dynamic t Text -> m ()
dynIconAttr m i = do
  let attr = ffor i $ \name -> m <> if T.null name then mempty else "class" =: ("fa fa-" <> name)
  elDynAttr "i" attr $ return ()

dynIcon2xAttr :: (DomBuilder t m, PostBuild t m) => Map Text Text -> Dynamic t Text -> m ()
dynIcon2xAttr m i = do
  let attr = ffor i $ \name -> m <> if T.null name then mempty else "class" =: ("fa fa-2x fa-" <> name)
  elDynAttr "i" attr $ return ()

dynIcon :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m ()
dynIcon = dynIconAttr mempty

dynIcon2x :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m ()
dynIcon2x = dynIcon2xAttr mempty

--  Type checked faIcon functions
faIcon :: DomBuilder t m => FontAwesome -> FontAwesomeConfig -> m ()
faIcon i conf = elClass "i" ((fontAwesomeClass i) <> " " <> (fontAwesomeConfigClass conf)) $ return ()
--TODO: This needs a clear fallback mechanism for when something isn't present in the FontAwesome type

--  faIcon prime functions
faIcon' :: DomBuilder t m => FontAwesome -> FontAwesomeConfig -> m (Element EventResult (DomBuilderSpace m) t, ())
faIcon' i conf = elClass' "i" ((fontAwesomeClass i) <> " " <> (fontAwesomeConfigClass conf)) $ return ()

faLg :: FontAwesomeConfig -> FontAwesomeConfig
faLg cfg = cfg { _fontAwesomeConfig_size = Size_Large }

fa2x :: FontAwesomeConfig -> FontAwesomeConfig
fa2x cfg = cfg { _fontAwesomeConfig_size = Size_2x }

fa3x :: FontAwesomeConfig -> FontAwesomeConfig
fa3x cfg = cfg { _fontAwesomeConfig_size = Size_3x }

fa4x :: FontAwesomeConfig -> FontAwesomeConfig
fa4x cfg = cfg { _fontAwesomeConfig_size = Size_4x }

fa5x :: FontAwesomeConfig -> FontAwesomeConfig
fa5x cfg = cfg { _fontAwesomeConfig_size = Size_5x }

#ifdef USE_TEMPLATE_HASKELL
makeLenses ''FontAwesomeConfig
#else
fontAwesomeConfig_size :: Lens' FontAwesomeConfig Size
fontAwesomeConfig_size f cfg = (\x -> cfg { _fontAwesomeConfig_size = x }) <$> f (_fontAwesomeConfig_size cfg)
{-# INLINE fontAwesomeConfig_size #-}

fontAwesomeConfig_fixedWidth :: Lens' FontAwesomeConfig Bool
fontAwesomeConfig_fixedWidth f cfg = (\x -> cfg { _fontAwesomeConfig_fixedWidth = x }) <$> f (_fontAwesomeConfig_fixedWidth cfg)
{-# INLINE fontAwesomeConfig_fixedWidth #-}

fontAwesomeConfig_border :: Lens' FontAwesomeConfig Bool
fontAwesomeConfig_border f cfg = (\x -> cfg { _fontAwesomeConfig_border = x }) <$> f (_fontAwesomeConfig_border cfg)
{-# INLINE fontAwesomeConfig_border #-}

fontAwesomeConfig_pull :: Lens' FontAwesomeConfig (Maybe Pull)
fontAwesomeConfig_pull f cfg = (\x -> cfg { _fontAwesomeConfig_pull = x }) <$> f (_fontAwesomeConfig_pull cfg)
{-# INLINE fontAwesomeConfig_pull #-}

fontAwesomeConfig_animation :: Lens' FontAwesomeConfig (Maybe Animation)
fontAwesomeConfig_animation f cfg = (\x -> cfg { _fontAwesomeConfig_animation = x }) <$> f (_fontAwesomeConfig_animation cfg)
{-# INLINE fontAwesomeConfig_animation #-}

fontAwesomeConfig_rotation :: Lens' FontAwesomeConfig (Maybe Rotation)
fontAwesomeConfig_rotation f cfg = (\x -> cfg { _fontAwesomeConfig_rotation = x }) <$> f (_fontAwesomeConfig_rotation cfg)
{-# INLINE fontAwesomeConfig_rotation #-}

fontAwesomeConfig_flip :: Lens' FontAwesomeConfig (Maybe Flip)
fontAwesomeConfig_flip f cfg = (\x -> cfg { _fontAwesomeConfig_flip = x }) <$> f (_fontAwesomeConfig_flip cfg)
{-# INLINE fontAwesomeConfig_flip #-}

fontAwesomeConfig_listIcon :: Lens' FontAwesomeConfig Bool
fontAwesomeConfig_listIcon f cfg = (\x -> cfg { _fontAwesomeConfig_listIcon = x }) <$> f (_fontAwesomeConfig_listIcon cfg)
{-# INLINE fontAwesomeConfig_listIcon #-}
#endif

-- Deprecated functions

{-# DEPRECATED icon "Instead of @icon \"some-icon\"@ use @faIcon FaSomeIcon def@" #-}
icon :: DomBuilder t m => Text -> m ()
icon i = elClass "i" ("fa fa-" <> i) $ return ()

{-# DEPRECATED iconlg "Instead of @iconlg \"some-icon\"@ use @faIcon FaSomeIcon (faLg def)@" #-}
iconlg :: DomBuilder t m => Text -> m ()
iconlg i = icon (i <> " fa-lg")
--TODO: What actually replaces this?  There doesn't seem to be a way to generate fa-lg above.

{-# DEPRECATED icon2x "Instead of @icon2x \"some-icon\"@ use @faIcon FaSomeIcon (fa2x def)@" #-}
icon2x :: DomBuilder t m => Text -> m ()
icon2x i = icon (i <> " fa-2x")

{-# DEPRECATED icon3x "Instead of @icon3x \"some-icon\"@ use @faIcon FaSomeIcon (fa3x def)@" #-}
icon3x :: DomBuilder t m => Text -> m ()
icon3x i = icon (i <> " fa-3x")

{-# DEPRECATED icon4x "Instead of @icon4x \"some-icon\"@ use @faIcon FaSomeIcon (fa4x def)@" #-}
icon4x :: DomBuilder t m => Text -> m ()
icon4x i = icon (i <> " fa-4x")

{-# DEPRECATED icon5x "Instead of @icon5x \"some-icon\"@ use @faIcon FaSomeIcon (fa5x def)@" #-}
icon5x :: DomBuilder t m => Text -> m ()
icon5x i = icon (i <> " fa-5x")

