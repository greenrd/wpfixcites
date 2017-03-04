{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NamedFieldPuns #-}
module Text.Mediawiki.Wikipedia (fixCites) where

import ClassyPrelude
import Control.Lens.Fold ((^?))
import Control.Lens.Iso (reversed)
import Control.Lens.Operators ((&))
import Control.Lens.Prism (_Just)
import Control.Lens.Setter ((%~))
import Control.Lens.TH (abbreviatedFields, makeLensesFor, makeLensesWith)
import Control.Lens.Tuple (_2)
import qualified Data.ByteString.Char8 as C
import Data.Char (isSpace)
import Data.Machine (construct, runT, SourceT, yield)
import Data.Machine.Concurrent.Scatter (scatter)
import Data.Monoid (Endo(..))
import Data.Text (strip)
import Data.Text.Lens (packed)
import Data.Tree.NTree.TypeDefs (NTree(..))
import Network.Mediawiki.API (parseTree)
import Network.Mediawiki.API.Lowlevel (APIConnection(..), defaultEndpoint)
import Network.HTTP.Client
       (getUri, hrFinalRequest, Manager, newManager, parseRequest,
        responseHeaders, withResponseHistory)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.URI (URI(..), URIAuth(..), uriToString)
import Network.Wreq.Session (Session, withAPISession)
import Text.Mediawiki.ParseTree
import Text.XML.HXT.PathFinder (findElements, hasLocalName, LocatedTree(..), Path(..))
import Text.XML.HXT.DOM.TypeDefs (XmlTree, XNode(..))

makeLensesFor [("uriAuthority", "authority")] ''URI
makeLensesWith abbreviatedFields ''URIAuth

data CiteJournal = CiteJournal { url :: String
                               , doi :: String
                               }

data CiteJournalL = CiteJournalL { urlL :: (Path, String)
                                 , doiL :: String
                                 }

extractL :: CiteJournalL -> (Path, CiteJournal)
extractL CiteJournalL { urlL = (path, url), doiL } =
  (path, CiteJournal { url, doi = doiL })

handleMixedContent :: [LocatedTree] -> Maybe (Path, String)
handleMixedContent [LocatedTree { path, tree = NTree (XText s) _ }] = Just (path, s)
handleMixedContent []  = Nothing
handleMixedContent list = error $ "Don't know how to handle children: " ++ show list

parseCiteJournalL :: TemplateInvocation -> Maybe CiteJournalL
parseCiteJournalL ti@TemplateCall { templateName, arguments }
  | eqStripped templateName "cite journal" = do
      urlL <- handleMixedContent =<< argument "url" ti
      doiL <- handleMixedContent =<< argument "doi" ti
      return CiteJournalL { urlL, doiL = snd doiL }
parseCiteJournalL _ = Nothing

applyFix :: (Path, String) -> XmlTree -> XmlTree
applyFix (Path [], newString) _ = NTree (XText newString) []
applyFix (Path (h:t), newString) (NTree content children) =
  NTree content $ before ++ applyFix (Path t, newString) focus : after
    where
      (before, focus : after) = splitAt h children

resolveDOI :: Manager -> String -> IO URI
resolveDOI manager doi = redirectTarget $ "http://dx.doi.org/" ++ doi
  where
    redirectTarget :: String -> IO URI
    redirectTarget url = do
      req <- parseRequest $ "HEAD " ++ url
      withResponseHistory req manager consume
        where
          consume = return . getUri . hrFinalRequest

fixCites :: String -> Text -> IO [Text]
fixCites urlPrefix pageName = withAPISession $ \session' ->
  do
    manager <- newManager tlsManagerSettings
    xmlTree <- runReaderT (parseTree pageName) $ APIConnection defaultEndpoint id session'
    let templateXml = findElements (hasLocalName "template") xmlTree
        templateCalls = mapMaybe unpickleL templateXml
        citeJournals = mapMaybe parseCiteJournalL templateCalls
        needFixing = extractL <$> filter ((urlPrefix `isPrefixOf`) . snd . urlL) citeJournals
        genSource :: CiteJournal -> SourceT IO String
        genSource CiteJournal { doi, url } = construct $ do
          newUrl <- liftIO . resolveDOI manager $ doi & packed %~ strip
          yield $ substPreservingWS newUrl url
    fixedUrls <- runT . scatter $ _2 genSource <$> needFixing
    let fixedXml = foldr applyFix xmlTree fixedUrls
    return $ toWikitext fixedXml

substPreservingWS :: URI -> String -> String
substPreservingWS uri str =
  let (pre, rest) = span isSpace str
      post = takeWhileEnd isSpace rest
  in  pre ++ uriToString id (removeUnnecessaryPorts uri) post

defaultPorts :: [(String, Int)]
defaultPorts = first (`snoc` ':') <$> [ ("http", 80)
                                      , ("https", 443)
                                      , ("ftp", 21)
                                      ]

removeUnnecessaryPorts :: URI -> URI
removeUnnecessaryPorts = removePortIfExists `applyFor` ((`lookup` defaultPorts) . uriScheme)

removePortIfExists :: Int -> Endo URI
removePortIfExists defaultPort =
  Endo $ authority . _Just . port %~ (pure (Endo mempty) `applyFor` check)
  where
    check :: String -> Maybe ()
    check = guard . (== ':' : show defaultPort)

applyFor :: MonoFoldable mono => (Element mono -> Endo a) -> (a -> mono) -> a -> a
applyFor f = (flip (appEndo . foldMap f) <*>)

takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd = (reversed %~) . takeWhile
