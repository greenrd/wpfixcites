module Network.Mediawiki.API where

import           ClassyPrelude
import           Control.Arrow.ListArrow (runLA)
import           Control.Lens
import           Data.Aeson.Lens (key, _String)
import           Network.Wreq (Options, responseBody)
import           Network.Mediawiki.API.Lowlevel
import           Text.XML.HXT.Arrow.ReadDocument (xreadDoc)
import           Text.XML.HXT.DOM.TypeDefs (XmlTree)

parseTree :: (Options -> Options) -> API XmlTree
parseTree opt = do
  resp <- apiGet $ \opts -> opts
    & mwParam "action" .~ ["parse"]
    & opt
    & mwParam "prop" .~ ["parsetree"]
  parse =<< (maybe (fail "wrong JSON") return $ (^? responseBody . key "parse" . key "parsetree" . _String) resp)
  where
    parse = extract . runLA xreadDoc . unpack
    extract [tree] = return tree
    extract _ = fail "ill-formed XML"

parseTreeOfPage :: Text -> API XmlTree
parseTreeOfPage pageName = parseTree $ mwParam "page" .~ [pageName]

parseTreeOfText :: Text -> API XmlTree
parseTreeOfText text = parseTree $ mwParam "text" .~ [text]
