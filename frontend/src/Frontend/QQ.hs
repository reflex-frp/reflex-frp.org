{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.QQ
  ( example
  , typeSignature
  , argPerLine
  , contextNewline
  ) where

import "template-haskell" Language.Haskell.TH
import Data.Maybe (mapMaybe)
import Data.Semigroup (Min(..))
import Data.Text (Text)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Language.Haskell.Exts as Exts
import qualified Language.Haskell.Meta as Meta

parseMode :: Exts.ParseMode
parseMode = Exts.defaultParseMode
    { Exts.baseLanguage = Exts.Haskell2010
    , Exts.fixities = Just $ concat
      [ Exts.preludeFixities
      , Exts.baseFixities
      , Exts.infixl_ 8 ["^?", "^."]
      , Exts.infixl_ 7 ["=:"]
      , Exts.infixr_ 4 ["%~", ".~", "?~", "<>~"]
      , Exts.infixl_ 1 ["&"]
      ]
    , Exts.extensions = Exts.EnableExtension <$> knownExtensions
    }

knownExtensions :: [Exts.KnownExtension]
knownExtensions =
  [ Exts.DataKinds
  , Exts.ExistentialQuantification
  , Exts.ExplicitForAll
  , Exts.GADTs
  , Exts.LambdaCase
  , Exts.MultiParamTypeClasses
  , Exts.RecordWildCards
  , Exts.RecursiveDo
  , Exts.ScopedTypeVariables
  , Exts.TypeApplications
  , Exts.TypeFamilies
  , Exts.TemplateHaskell
  ]

example :: QuasiQuoter
example = QuasiQuoter
  { quoteExp = \ex -> [|
    ( unindent ex
    , $(return $ Meta.toExp $ Exts.fromParseResult $ Exts.parseExpWithMode parseMode $ "do\n" <> ex)
    )
  |]
  , quotePat = const $ error "ex: not an expression"
  , quoteType = const $ error "ex: not an expression"
  , quoteDec = const $ error "ex: not an expression"
  }

-- | Unindent a block of text
unindent :: Text -> Text
unindent t = T.unlines . map (T.drop $ minimumIndent t) $ T.lines t

-- | Get minimum indentation of a line
minimumIndent :: Text -> Int
minimumIndent = getMin . foldMap Min . mapMaybe lineIndent . T.lines

-- | Get the indentation of a line
lineIndent :: Text -> Maybe Int
lineIndent t
  | T.null (T.stripEnd t) = Nothing
  | otherwise = Just . T.length . T.takeWhile C.isSpace $ T.stripEnd t

-- Simple typeahead
-- https://gist.github.com/ali-abrar/3c5351398b46418fb6be
-- domRenderHook?
-- https://gist.github.com/ali-abrar/d2976cb701cb154b72888a4632e7ca1e
unqualify :: String -> String
unqualify "" = ""
unqualify (c:s)
  | C.isUpper c = case span C.isAlpha (c:s) of
    (_, '.':rest) -> unqualify rest
    (taken, rest) -> taken <> unqualify rest
  | otherwise = c : unqualify s

-- | Renamed type variables like a_2 to a
stripNumbers :: String -> String
stripNumbers "" = ""
stripNumbers (x:'_':a:b:c:rest)
  | C.isAlphaNum x && C.isNumber a && C.isNumber b && C.isNumber c = x : stripNumbers rest
stripNumbers (x:'_':a:b:rest)
  | C.isAlphaNum x && C.isNumber a && C.isNumber b = x : stripNumbers rest
stripNumbers (x:'_':a:rest)
  | C.isAlphaNum x && C.isNumber a = x : stripNumbers rest
stripNumbers (x:rest) = x : stripNumbers rest

-- | Strip types (most likely kinds) from a signature
stripTypes :: String -> String
stripTypes "" = ""
stripTypes ('(':t:' ':':':':':' ':rest)
  | C.isAlpha t = t:' ': stripTypes (drop 1 $ dropWhile (/= ')') rest)
stripTypes ('(':t:a:' ':':':':':' ':rest)
  | C.isAlpha t && C.isAlphaNum a = t:a:' ': stripTypes (drop 1 $ dropWhile (/= ')') rest)
stripTypes (x:rest) = x : stripTypes rest

-- | Strip a forall statement from a type signature
stripForAll :: String -> String
stripForAll "" = ""
stripForAll ('f':'o':'r':'a':'l':'l':rest) = stripForAll $ drop 1 $ dropWhile (/= '.') rest
stripForAll (x:rest) = x : stripForAll rest

-- | Add newlines before record fields
recordNewlines :: String -> String
recordNewlines "" = ""
recordNewlines ('{':' ':rest) = "\n  { " <> recordNewlines rest
recordNewlines ('{':rest) = "\n  { " <> recordNewlines rest
recordNewlines ('}':rest) = "\n  }" <> recordNewlines rest
recordNewlines (',':rest) = "\n  ," <> recordNewlines rest
recordNewlines (x:rest) = x : recordNewlines rest

-- | Squash consecutive whitespace
squash :: String -> String
squash (' ' : ' ' : rest) = squash $ ' ' : rest
squash (x : rest) = x : squash rest
squash [] = []

-- | Split type signature across lines. Won't work for function arguments.
argPerLine :: String -> String
argPerLine ('=' : '>' : rest) = '\n' : ' ' : ' ' : '=' : '>' : argPerLine rest
argPerLine ('-' : '>' : rest) = '\n' : ' ' : ' ' : '-' : '>' : argPerLine rest
argPerLine (x : rest) = x : argPerLine rest
argPerLine [] = []

-- | Insert a newline after the context
contextNewline :: String -> String
contextNewline ('=' : '>' : rest) = '\n' : ' ' : ' ' : '=' : '>' : contextNewline rest
contextNewline (x : rest) = x : contextNewline rest
contextNewline [] = []

-- | Pretty print the definition of a haskell type
typeSignature :: (String -> String) -> Name -> ExpQ
typeSignature postproc name = do
  let process = postproc . squash . stripForAll . stripTypes . stripNumbers . unqualify . unwords . lines
  str <- reify name >>= \case
    ClassOpI _ ty _ -> pure . process $ show name <> " :: " <> pprint ty
    VarI _ ty _ -> pure . process $ show name <> " :: " <> pprint ty
    TyConI d -> pure . recordNewlines . process $ pprint d
    i -> fail $ "typeSignature: Not handled: " <> show i
  litE $ stringL str
