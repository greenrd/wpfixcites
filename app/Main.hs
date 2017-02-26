module Main where

import ClassyPrelude hiding ((<>))
import Options.Applicative
import Text.Mediawiki.Wikipedia (fixCites)

data WpFixCitesArgs = WpFixCitesArgs { urlPrefix :: String
                                     , pageName  :: String
                                     }

urlPrefixOption =
  strOption
   ( long "url-prefix"
  <> short 'u'
  <> metavar "URL-PREFIX"
  <> help "Operate on all URLs that start with URL-PREFIX, assuming them to be dead." )

pageNameArg =
  argument str (metavar "PAGENAME")

myArgParser :: Parser WpFixCitesArgs
myArgParser = WpFixCitesArgs <$> urlPrefixOption <*> pageNameArg

main :: IO ()
main = do
  args <- execParser . info (helper <*> myArgParser) $
    fullDesc <> progDesc "Fixes broken citations on Wikipedia" <>
    header "wpfixcites"
  fixed <- fixCites (urlPrefix args) . pack $ pageName args
  mapM_ putStr fixed
