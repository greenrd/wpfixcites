{-# LANGUAGE NamedFieldPuns #-}
module Text.Mediawiki.ParseTree where

import ClassyPrelude
import Data.Text (strip)
import Data.Tree.NTree.TypeDefs (NTree(..))
import Text.XML.HXT.DOM.QualifiedName (QName)
import Text.XML.HXT.DOM.TypeDefs (XmlTree, XmlTrees, XNode(..))
import Text.XML.HXT.PathFinder (hasLocalName, LocatedTree(..), number, Numbered, Path(..))

class LocatingUnpickle a where
  unpickleL :: LocatedTree -> Maybe a

data TemplateArgument = TemplateArg { argName :: String
                                    , argValue :: [LocatedTree]
                                    }

data TemplateInvocation = TemplateCall { templateName :: String
                                       , arguments :: [TemplateArgument]
                                       }
eqStripped :: String -> Text -> Bool
eqStripped s t = strip (pack $ toLower s) == t

argument :: Text -> TemplateInvocation -> Maybe [LocatedTree]
argument name TemplateCall { arguments } =
  argValue <$> find (\a -> eqStripped (argName a) name) arguments

named :: String -> XmlTrees -> [Numbered XmlTree]
named name = filter (maybe False (hasLocalName name) . elemName . snd) . number

exactly1 :: [a] -> Maybe a
exactly1 [x] = Just x
exactly1 _   = Nothing

named1 :: String -> XmlTrees -> Maybe (Int, XmlTree)
named1 name = exactly1 . named name

instance LocatingUnpickle TemplateArgument where
  unpickleL (LocatedTree argPath (NTree (XTag _ _) children)) = do
    (_, NTree _ [NTree (XText argName) _]) <- named1 "name" children
    (valueBranch, NTree _ valueTrees) <- named1 "value" children
    let toLocated branch tree =
          LocatedTree { path = Path $ branches argPath ++ [valueBranch, branch]
                      , tree
                      }
    return TemplateArg { argName
                       , argValue = uncurry toLocated <$> number valueTrees
                       }

instance LocatingUnpickle TemplateInvocation where
  unpickleL (LocatedTree invocationPath (NTree (XTag _ _) children)) = do
    (_, NTree _ [NTree (XText title) _]) <- named1 "title" children
    let partElems = named "part" children
    tas <- mapM (unpickleL . uncurry toLocated) partElems
    return TemplateCall { templateName = title
                        , arguments = tas
                        }
      where
        toLocated branch tree =
          LocatedTree { path = Path $ snoc (branches invocationPath) branch
                      , tree
                      }
  unpickleL _ = Nothing

elemName :: NTree XNode -> Maybe QName
elemName (NTree (XTag n _) _) = Just n
elemName _ = Nothing

toWikitext :: XmlTree -> [Text]
toWikitext (NTree (XText t) _) = [pack t]
toWikitext (NTree (XEntityRef er) _) = ["&", pack er, ";"]
toWikitext (NTree (XCmt cmt) _) = ["<!--", pack cmt, "-->"]
toWikitext (NTree (XCdata t) _) = [pack t]
toWikitext (NTree (XTag name _) children)
  | hasLocalName "template" name = "{{" : (toWikitext =<< children) ++ ["}}"]
toWikitext (NTree (XTag name _) children)
  | hasLocalName "part" name = "|" : (toWikitext =<< children)
toWikitext (NTree (XTag name _) children)
  | hasLocalName "ext" name = "<" : (toWikitext =<< children)
toWikitext (NTree (XTag name _) children)
  | hasLocalName "inner" name = ">" : (toWikitext =<< children)
toWikitext (NTree (XTag _ _) children)
  = toWikitext =<< children
toWikitext (NTree _ _) = []
