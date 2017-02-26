{-# LANGUAGE Rank2Types #-}
module Control.Lens.Text.Extras where

import ClassyPrelude hiding (intercalate)
import Control.Lens.Iso
import Data.Text (intercalate, splitOn)

-- | Isomorphism between a delimited text and its constituent parts
delimited :: Text -> Iso' Text [Text]
delimited = uncurry iso . (splitOn &&& intercalate)
