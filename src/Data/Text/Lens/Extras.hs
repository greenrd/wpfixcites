{-# LANGUAGE Rank2Types #-}
module Data.Text.Lens.Extras where

import ClassyPrelude hiding (intercalate)
import Control.Lens.Iso
import Control.Lens.Lens (Lens')
import Data.Char (isSpace)
import Data.Text (intercalate, splitOn)

-- | Isomorphism between a delimited text and its constituent parts
delimited :: Text -> Iso' Text [Text]
delimited = uncurry iso . (splitOn &&& intercalate)

-- | Lens from a text possibly surrounded by whitespace, to its stripped equivalent
stripped :: Lens' Text Text
stripped afb surrounded = replaceNonWS <$> afb got
  where
    (pre, rest) = span isSpace surrounded
    (got, post) = spanEnd isSpace rest
    replaceNonWS :: Text -> Text
    replaceNonWS newInner = pre ++ newInner ++ post

spanEnd :: (Char -> Bool) -> Text -> (Text, Text)
spanEnd p = (reverse *** reverse) . swap . span p . reverse
