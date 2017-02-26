{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NamedFieldPuns #-}
module Text.Mediawiki.Wikipedia (fixCites) where

import ClassyPrelude
import Control.Lens ((^?))
import qualified Data.ByteString.Char8 as C
import Data.Char (isSpace)
import Data.Machine (construct, runT, SourceT, yield)
import Data.Machine.Concurrent.Scatter (scatter)
import Data.Text (strip)
import Data.Tree.NTree.TypeDefs (NTree(..))
import Network.Mediawiki.API (parseTree)
import Network.Mediawiki.API.Lowlevel (APIConnection(..), defaultEndpoint)
import Network.HTTP.Client
       (defaultManagerSettings, getUri, hrFinalRequest, Manager, newManager, parseRequest,
        responseHeaders, withResponseHistory)
import Network.URI (URI, uriToString)
import Network.Wreq.Session (Session, withAPISession)
import Text.Mediawiki.ParseTree
import Text.XML.HXT.PathFinder (findElements, hasLocalName, LocatedTree(..), Path(..))
import Text.XML.HXT.DOM.TypeDefs (XmlTree, XNode(..))

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

maxRedirects :: Int
maxRedirects = 10

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
    manager <- newManager defaultManagerSettings
    xmlTree <- runReaderT (parseTree pageName) $ APIConnection defaultEndpoint id session'
    let templateXml = findElements (hasLocalName "template") xmlTree
        templateCalls = mapMaybe unpickleL templateXml
        citeJournals = mapMaybe parseCiteJournalL templateCalls
        needFixing = extractL <$> filter ((urlPrefix `isPrefixOf`) . snd . urlL) citeJournals
        genSource :: CiteJournal -> SourceT IO String
        genSource CiteJournal { doi, url } = construct $ do
          newUrl <- liftIO . resolveDOI manager . unpack . strip $ pack doi
          yield $ substPreservingWS newUrl url
    fixedUrls <- runT . scatter $ wrap genSource <$> needFixing
    let fixedXml = foldr applyFix xmlTree fixedUrls
    return $ toWikitext fixedXml
      where
        wrap :: (a -> SourceT IO b) -> (c, a) -> SourceT IO (c, b)
        wrap f (c, x) = (c, ) <$> f x

substPreservingWS :: URI -> String -> String
substPreservingWS uri str =
  let (pre, rest) = span isSpace str
      post = takeWhileEnd isSpace rest
  in  pre ++ uriToString id uri post

takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p = reverse . takeWhile p . reverse
