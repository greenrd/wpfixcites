module Text.Mediawiki.ParseTree.Ref where

import ClassyPrelude
import Control.Lens.Iso (iso, Iso')
import Control.Lens.Traversal (Traversal')
import Data.Tree.NTree.TypeDefs (NTree(..))
import Text.XML.HXT.DOM.QualifiedName (QName)
import Text.XML.HXT.DOM.TypeDefs (XmlTree, XmlTrees, XNode(..))
import Text.XML.HXT.PathFinder (hasLocalName)

newtype Ref = Ref { innerComponent :: XmlTree }

refIso :: Iso' XmlTree Ref
refIso = iso Ref innerComponent

getText :: XmlTree -> String
getText (NTree (XText s) _) = s
getText _ = error "Don't know how to handle"

isNameElement :: String -> XmlTree -> Bool
isNameElement name (NTree (XTag qName _) children)
  | hasLocalName "name" qName = (getText =<< children) == name
isNameElement _ _ = False

isExt :: String -> QName -> XmlTrees -> Bool
isExt extName qName children =
  hasLocalName "ext" qName && any (isNameElement extName) children

innerTraversal :: Traversal' XmlTree Ref
innerTraversal f (NTree t@(XTag qName _) children)
  | hasLocalName "inner" qName = NTree t <$> traverse (refIso f) children
innerTraversal _ t = pure t

innerTraversalL :: Traversal' XmlTrees Ref
innerTraversalL f = traverse (innerTraversal f)

refTraversal :: Traversal' XmlTree Ref
refTraversal f (NTree t@(XTag qName _) children)
  | isExt "ref" qName children = NTree t <$> innerTraversalL f children
refTraversal _ t = pure t
