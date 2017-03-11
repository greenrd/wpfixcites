{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RankNTypes        #-}
module Network.Mediawiki.API.Lowlevel where

import           ClassyPrelude
import           Control.Lens hiding (unsnoc)
import           Data.Text.Lens.Extras    (delimited)
import           Data.Time.Clock          (DiffTime, secondsToDiffTime)
import           Network.Wreq             (Options, Response, defaults)
import           Network.Wreq.Lens        (param, responseBody)
import           Network.Wreq.Session

-- | Setter which sets (and creates if necessary) the last element of a list
replacingLast :: a -> Setter' [a] a
replacingLast deflt = sets $ \f -> maybe [f deflt] (\(xs, x) -> xs ++ [f x]) . unsnoc

-- | Parameter that follows Mediawiki multi-parameter encoding rules
mwParam :: Text -> Setter' Options [Text]
mwParam name = param name . replacingLast "" . delimited "|"

data APIConnection = APIConnection { endpoint       :: String
                                   , optsCustomiser :: Options -> Options
                                   , session        :: Session }

defaultEndpoint :: String
defaultEndpoint = "https://en.wikipedia.org/w/api.php"

defaultMaxLag :: DiffTime
defaultMaxLag = secondsToDiffTime 1

maxLag :: DiffTime -> Options -> Options
maxLag ml = mwParam "maxlag" .~ [pack . show . floor $ toRational ml]

type API = ReaderT APIConnection IO

-- | Makes any @GET@ request to the MediaWiki API
apiGet :: (Options -> Options) -> API (Response ByteString)
apiGet optsFn = do
   apiConnection <- ask
   r <- liftIO $ getWith (defaults & mwParam "format" .~ ["json"] & mwParam "formatversion" .~ ["2"] &
                          maxLag defaultMaxLag &
                          optsCustomiser apiConnection & optsFn)
                         (session apiConnection)
                         (endpoint apiConnection)
   return $ over responseBody toStrict r
