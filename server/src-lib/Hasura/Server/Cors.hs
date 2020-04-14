{-# LANGUAGE DeriveAnyClass #-}

-- | CORS (Cross Origin Resource Sharing) related configuration

module Hasura.Server.Cors
  ( CorsConfig (..)
  , CorsPolicy (..)
  , parseOrigin
  , readCorsDomains
  , mkDefaultCorsPolicy
  , isCorsDisabled
  , Domains (..)
  , inWildcardList
  ) where

import           Hasura.Prelude
import qualified Data.Bifunctor       as Bifunctor

import           Control.Applicative  (optional)

import qualified Data.Aeson           as J
import qualified Data.Aeson.Casing    as J
import qualified Data.Aeson.TH        as J
import qualified Data.Attoparsec.Text as AT
import qualified Data.HashSet         as Set
import qualified Data.Text            as T


data DomainParts =
  DomainParts
  { wdScheme :: !Text
  , wdHost   :: !Text -- the hostname part (without the *.)
  , wdPort   :: !(Maybe Int)
  } deriving (Show, Eq, Generic, Hashable)

$(J.deriveToJSON (J.aesonDrop 2 J.snakeCase) ''DomainParts)

data Domains
  = Domains
  { dmFqdns     :: !(Set.HashSet Text)
  , dmWildcards :: !(Set.HashSet DomainParts)
  } deriving (Show, Eq)

$(J.deriveToJSON (J.aesonDrop 2 J.snakeCase) ''Domains)

data CorsConfig
  = CCAllowAll
  | CCAllowedOrigins Domains
  | CCDisabled Bool -- should read cookie?
  deriving (Show, Eq)

instance J.ToJSON CorsConfig where
  toJSON c = case c of
    CCDisabled wsrc    -> toJ True J.Null (Just wsrc)
    CCAllowAll         -> toJ False (J.String "*") Nothing
    CCAllowedOrigins d -> toJ False (J.toJSON d) Nothing
    where
      toJ :: Bool -> J.Value -> Maybe Bool -> J.Value
      toJ dis origs mWsRC =
        J.object [ "disabled" J..= dis
                 , "ws_read_cookie" J..= mWsRC
                 , "allowed_origins" J..= origs
                 ]

isCorsDisabled :: CorsConfig -> Bool
isCorsDisabled = \case
  CCDisabled _ -> True
  _          -> False

readCorsDomains :: String -> Either String CorsConfig
readCorsDomains str
  | str == "*"               = pure CCAllowAll
  | otherwise = do
      let domains = map T.strip $ T.splitOn "," (T.pack str)
      pDomains <- mapM parseOptWildcardDomain domains
      let (fqdns, wcs) = (lefts pDomains, rights pDomains)
      return $ CCAllowedOrigins $ Domains (Set.fromList fqdns) (Set.fromList wcs)


data CorsPolicy
  = CorsPolicy
  { cpConfig  :: !CorsConfig
  , cpMethods :: ![Text]
  , cpMaxAge  :: !Int
  } deriving (Show, Eq)

mkDefaultCorsPolicy :: CorsConfig -> CorsPolicy
mkDefaultCorsPolicy cfg =
  CorsPolicy
  { cpConfig = cfg
  , cpMethods = ["GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"]
  , cpMaxAge = 1728000
  }

inWildcardList :: Domains -> Text -> Bool
inWildcardList (Domains _ wildcards) origin =
  either (const False) (`Set.member` wildcards) $ parseOrigin origin


-- | Parsers for wildcard domains

runParser :: AT.Parser a -> Text -> Either String a
runParser = AT.parseOnly

parseOrigin :: Text -> Either String DomainParts
parseOrigin = runParser originParser

originParser :: AT.Parser DomainParts
originParser =
  domainParser (Just ignoreSubdomain)
  where ignoreSubdomain = do
          s <- AT.takeTill (== '.')
          void $ AT.char '.'
          return s

parseOptWildcardDomain :: Text -> Either String (Either Text DomainParts)
parseOptWildcardDomain d =
  Bifunctor.first (const errMsg) $ runParser optWildcardDomainParser d
  where

    optWildcardDomainParser :: AT.Parser (Either Text DomainParts)
    optWildcardDomainParser =
      Right <$> wildcardDomainParser <|> Left <$> fqdnParser

    errMsg = "invalid domain: '" <> T.unpack d <> "'. " <> helpMsg
    helpMsg = "All domains should have scheme + (optional wildcard) host + "
              <> "(optional port)"

    wildcardDomainParser :: AT.Parser DomainParts
    wildcardDomainParser = domainParser $ Just (AT.string "*" *> AT.string ".")

    fqdnParser :: AT.Parser Text
    fqdnParser = do
      (DomainParts scheme host port) <- domainParser Nothing
      let sPort = maybe "" (\p -> ":" <> T.pack (show p)) port
      return $ scheme <> host <> sPort


domainParser :: Maybe (AT.Parser Text) -> AT.Parser DomainParts
domainParser parser = do
  scheme <- schemeParser
  forM_ parser void
  host <- hostPortParser
  port <- optional portParser
  return $ DomainParts scheme host port

  where
    schemeParser :: AT.Parser Text
    schemeParser = AT.string "http://" <|> AT.string "https://"

    hostPortParser :: AT.Parser Text
    hostPortParser = hostWithPortParser <|> AT.takeText

    hostWithPortParser :: AT.Parser Text
    hostWithPortParser = do
      h <- AT.takeWhile1 (/= ':')
      void $ AT.char ':'
      return h

    portParser :: AT.Parser Int
    portParser = AT.decimal
