module Network.Mediawiki.API where

import           ClassyPrelude
import           Control.Arrow.ListArrow (runLA)
import           Control.Lens
import           Data.Aeson.Lens (key, _String)
import           Data.Aeson.Types
import           Data.Machine.Type (MachineT)
import           Network.Wreq (Response, responseBody)
import           Network.Mediawiki.API.Lowlevel
import           Text.XML.HXT.Arrow.ReadDocument (xreadDoc)
import           Text.XML.HXT.DOM.TypeDefs (XmlTree)

parseTree :: Text -> API XmlTree
parseTree pageName = do
  resp <- apiGet $ \opts -> opts
    & mwParam "action" .~ ["parse"]
    & mwParam "page" .~ [pageName]
    & mwParam "prop" .~ ["parsetree"]
  parse =<< (maybe (fail "wrong JSON") return $ (^? responseBody . key "parse" . key "parsetree" . _String) resp)
  where
    parse = extract . runLA xreadDoc . unpack
    extract [tree] = return tree
    extract _ = fail "ill-formed XML"
