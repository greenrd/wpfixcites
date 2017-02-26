{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
module Text.XML.HXT.PathFinder where

import ClassyPrelude
import Data.Tree.NTree.TypeDefs (NTree(..))
import Text.XML.HXT.DOM.QualifiedName (localPart)
import Text.XML.HXT.DOM.TypeDefs (QName, XmlTree, XNode(..))

newtype Path = Path { branches :: [Int] } deriving (Monoid, Show)

data LocatedTree = LocatedTree { path :: Path
                               , tree :: XmlTree
                               } deriving (Show)

type Numbered a = (Int, a)

number :: [a] -> [Numbered a]
number = zip [0..]

-- Does not search for elements nested within matching elements.
findElements :: (QName -> Bool) -> XmlTree -> [LocatedTree]
findElements qNamePred = finder mempty
  where
    finder :: Path -> XmlTree -> [LocatedTree]
    finder path tree@(NTree (XTag qName _) _)
      | qNamePred qName = [LocatedTree { path = Path . reverse $ branches path, tree }]
    finder path (NTree (XTag _ _) children)
      = uncurry stepDown =<< number children
      where
        stepDown branch = finder . Path $ branch : branches path
    finder _ _ = []

hasLocalName :: String -> QName -> Bool
hasLocalName name qName = localPart qName == name
