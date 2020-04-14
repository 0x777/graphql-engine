module Hasura.HTTP
  ( wreqOptions
  , HttpException(..)
  , httpExceptToJSON
  , hdrsToText
  , addDefaultHeaders
  , HttpResponse(..)
  , getSetCookieHeaders
  , getRequestHeader
  , RequestId
  , getRequestId
  , mkRequestIdHeader
  , gzipHeader
  , sqlHeader
  , jsonHeader
  , mkUpstreamRequestHeaders
  , commonClientHeadersIgnored
  ) where

import           Hasura.Prelude

import           Control.Lens          hiding ((.=))
import           Data.CaseInsensitive  (original)
import           Data.Text.Conversions (UTF8 (..), convertText)

import qualified Data.Aeson            as J
import qualified Data.ByteString       as B
import qualified Data.Text             as T
import qualified Network.HTTP.Client   as HTTP
import qualified Network.HTTP.Types    as HTTP
import qualified Network.Wreq          as Wreq
import qualified Data.UUID             as UUID
import qualified Data.UUID.V4          as UUID

import           Hasura.Server.Version (HasVersion, currentVersion)
import           Hasura.RQL.Types.Permission (isUserVar)

import qualified Data.HashSet as Set
import qualified Data.CaseInsensitive as CI

hdrsToText :: [HTTP.Header] -> [(Text, Text)]
hdrsToText hdrs =
  [ (bsToTxt $ original hdrName, bsToTxt hdrVal)
  | (hdrName, hdrVal) <- hdrs
  ]

wreqOptions :: HasVersion => HTTP.Manager -> [HTTP.Header] -> Wreq.Options
wreqOptions manager hdrs =
  Wreq.defaults
  & Wreq.headers .~  addDefaultHeaders hdrs
  & Wreq.checkResponse ?~ (\_ _ -> return ())
  & Wreq.manager .~ Right manager

-- Adds defaults headers overwriting any existing ones
addDefaultHeaders :: HasVersion => [HTTP.Header] -> [HTTP.Header]
addDefaultHeaders hdrs = defaultHeaders <> rmDefaultHeaders hdrs
  where
    rmDefaultHeaders = filter (not . isDefaultHeader)

isDefaultHeader :: HasVersion => HTTP.Header -> Bool
isDefaultHeader (hdrName, _) = hdrName `elem` (map fst defaultHeaders)

defaultHeaders :: HasVersion => [HTTP.Header]
defaultHeaders = [contentType, userAgent]
  where
    contentType = ("Content-Type", "application/json")
    userAgent   = ( "User-Agent"
                  , "hasura-graphql-engine/" <> unUTF8 (convertText currentVersion)
                  )

newtype HttpException
  = HttpException
  { unHttpException :: HTTP.HttpException }
  deriving (Show)

instance J.ToJSON HttpException where
  toJSON = \case
    (HttpException (HTTP.InvalidUrlException _ e)) ->
      J.object [ "type" J..= ("invalid_url" :: Text)
               , "message" J..= e
               ]
    (HttpException (HTTP.HttpExceptionRequest _ cont)) ->
      J.object [ "type" J..= ("http_exception" :: Text)
               , "message" J..= show cont
               ]

-- json representation of HTTP exception
httpExceptToJSON :: HTTP.HttpException -> J.Value
httpExceptToJSON e = case e of
  HTTP.HttpExceptionRequest x c ->
      let reqObj = J.object
            [ "host" J..= bsToTxt (HTTP.host x)
            , "port" J..= show (HTTP.port x)
            , "secure" J..= HTTP.secure x
            , "path" J..= bsToTxt (HTTP.path x)
            , "method" J..= bsToTxt (HTTP.method x)
            , "proxy" J..= (showProxy <$> HTTP.proxy x)
            , "redirectCount" J..= show (HTTP.redirectCount x)
            , "responseTimeout" J..= show (HTTP.responseTimeout x)
            , "requestVersion" J..= show (HTTP.requestVersion x)
            ]
          msg = show c
      in J.object ["request" J..= reqObj, "message" J..= msg]
  _        -> J.toJSON $ show e
  where
    showProxy (HTTP.Proxy h p) =
      "host: " <> bsToTxt h <> " port: " <> T.pack (show p)

data HttpResponse a
  = HttpResponse
  { _hrBody    :: !a
  , _hrHeaders :: !HTTP.ResponseHeaders
  } deriving (Functor, Foldable, Traversable)

getSetCookieHeaders :: Wreq.Response a -> HTTP.ResponseHeaders
getSetCookieHeaders resp =
  map (headerName,) $ resp ^.. Wreq.responseHeader headerName
  where
    headerName = "Set-Cookie"

newtype RequestId
  = RequestId { unRequestId :: Text }
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

getRequestHeader :: HTTP.HeaderName -> [HTTP.Header] -> Maybe B.ByteString
getRequestHeader hdrName hdrs = snd <$> mHeader
  where
    mHeader = find (\h -> fst h == hdrName) hdrs

getRequestId :: (MonadIO m) => [HTTP.Header] -> m RequestId
getRequestId headers =
  -- generate a request id for every request if the client has not sent it
  case getRequestHeader requestIdHeader headers  of
    Nothing    -> RequestId . UUID.toText <$> liftIO UUID.nextRandom
    Just reqId -> return $ RequestId $ bsToTxt reqId

requestIdHeader :: HTTP.HeaderName
requestIdHeader = "x-request-id"

mkRequestIdHeader :: RequestId -> HTTP.Header
mkRequestIdHeader requestId =
  (requestIdHeader, txtToBs $ unRequestId requestId)

gzipHeader :: HTTP.Header
gzipHeader = ("Content-Encoding", "gzip")

sqlHeader :: HTTP.Header
sqlHeader = ("Content-Type", "application/sql; charset=utf-8")

jsonHeader :: HTTP.Header
jsonHeader = ("Content-Type", "application/json; charset=utf-8")

-- | when forwarding headers that are received as part of a request
-- to an upstream server, we'll need to ignore certain headers and add
-- appropriate 'x-forwarded-' headers
-- mkUpstreamRequestHeaders :: [HTTP.Header] -> [HTTP.Header]
-- mkUpstreamRequestHeaders reqHeaders =
--   mapMaybe transformHeader reqHeaders
--   where
--     transformHeader :: HTTP.Header -> Maybe HTTP.Header
--     transformHeader (headerName, headerValue) =
--       if
--        | isUserVar $ bsToTxt $ CI.original headerName       -> Nothing
--        | headerName `Set.member` commonClientHeadersIgnored -> Nothing
--        | headerName == "Host" -> Just ("X-Forwarded-Host", headerValue)
--        | headerName == "User-Agent" -> Just ("X-Forwarded-User-Agent", headerValue)
--        | otherwise -> Just (headerName, headerValue)
--       where
--           -- ignore the following request headers from the client
--         commonClientHeadersIgnored :: Set.HashSet HTTP.HeaderName
--         commonClientHeadersIgnored = Set.fromList
--           [ "Content-Length", "Content-MD5", "Host", "User-Agent"
--           , "Origin", "Referer" , "Accept", "Accept-Encoding"
--           , "Accept-Language", "Accept-Datetime"
--           , "Cache-Control", "Connection", "DNT", "Content-Type"
--           ]

-- ignore the following request headers from the client
commonClientHeadersIgnored :: (IsString a) => [a]
commonClientHeadersIgnored =
  [ "Content-Length", "Content-MD5", "User-Agent", "Host"
  , "Origin", "Referer" , "Accept", "Accept-Encoding"
  , "Accept-Language", "Accept-Datetime"
  , "Cache-Control", "Connection", "DNT", "Content-Type"
  ]

mkUpstreamRequestHeaders :: [HTTP.Header] -> [HTTP.Header]
mkUpstreamRequestHeaders reqHeaders =
  xForwardedHeaders <> (filterUserVars . filterRequestHeaders) reqHeaders
  where
    filterUserVars = filter (\(k, _) -> not $ isUserVar $ bsToTxt $ CI.original k)
    xForwardedHeaders = flip mapMaybe reqHeaders $ \(hdrName, hdrValue) ->
      case hdrName of
        "Host"       -> Just ("X-Forwarded-Host", hdrValue)
        "User-Agent" -> Just ("X-Forwarded-User-Agent", hdrValue)
        _            -> Nothing

filterRequestHeaders :: [HTTP.Header] -> [HTTP.Header]
filterRequestHeaders =
  filterHeaders $ Set.fromList commonClientHeadersIgnored

filterHeaders :: Set.HashSet HTTP.HeaderName -> [HTTP.Header] -> [HTTP.Header]
filterHeaders list = filter (\(n, _) -> not $ n `Set.member` list)
