{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module Incredible.Config where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as Text
import Database.Redis qualified as Redis
import System.Exit (exitFailure)
import Toml qualified
import Toml.Schema qualified as Toml
import Incredible.Data
import qualified Data.ByteString as BS

readIncredibleConfig :: FilePath -> IO IncredibleConfig
readIncredibleConfig configFp = do
  tomlContents <- Text.readFile configFp
  case Toml.decode' tomlContents of
    Toml.Failure errors -> do
      putStrLn $ "Errors decoding toml config file " <> configFp <> " " <> prettyShowTomlError errors
      exitFailure
    Toml.Success warnings res -> do
      putStrLn $ "Warnings decoding toml config file " <> configFp <> " " <> prettyShowTomlError warnings
      pure res
  where
    prettyShowTomlError = show . fmap Toml.prettyDecodeError

data IncredibleConfig
  = IncredibleConfig
    { incredibleWebConfig :: IncredibleWebConfig
    , incredibleCacheConfig :: IncredibleCacheConfig
    , incredibleRedis :: Maybe IncredibleRedisConfig
    , incredibleMods :: HashMap UserName UserDigest
    }
  deriving (Show)

instance Toml.FromValue IncredibleConfig where
  fromValue = Toml.parseTableFromValue $
    IncredibleConfig
      <$> Toml.reqKey "web"
      <*> Toml.reqKey "cache"
      <*> Toml.optKey "redis"
      <*> (fmap (HashMap.fromList . Map.toList) $ Toml.reqKey "mods")

data IncredibleWebConfig
  = IncredibleWebConfig
    { incredibleWebPort :: Int
    , incredibleBaseUrl :: Text
    , incredibleOrigins :: [BS.ByteString]
    } deriving (Show)

instance Toml.FromValue IncredibleWebConfig where
  fromValue = Toml.parseTableFromValue $ IncredibleWebConfig <$> Toml.reqKey "port" <*> Toml.reqKey "base_url" <*> fmap (fmap TE.encodeUtf8) (Toml.reqKey "origins")

data IncredibleCacheConfig = IncredibleCacheConfig {
  incredibleMachineByVersion  :: Maybe Integer
, incrediblePuzzlesByVersion  :: Maybe Integer
, incredibleBlueprintByID     :: Maybe Integer
, incredibleDeltaCache        :: Maybe Integer
, incredibleSnapshotByID      :: Maybe Integer
, incredibleBlueprint2Snapshot :: Maybe Integer
} deriving (Show)

instance Toml.FromValue IncredibleCacheConfig where
  fromValue = Toml.parseTableFromValue $
    IncredibleCacheConfig
      <$> Toml.optKey "machines"
      <*> Toml.optKey "puzzles"
      <*> Toml.optKey "blueprints"
      <*> Toml.optKey "deltas"
      <*> Toml.optKey "snapshots"
      <*> Toml.optKey "blueprint2puzzle"

-- This contains all of the components of a redis connection because
-- the certificate store has to be fetched to create the tls connection
-- and that can't happen when parsing the config file
data IncredibleRedisConfig
  = IncredibleRedisConfig
    { incredibleRedisHostName :: String
    , incredibleRedisPort :: Redis.PortID
    , incredibleRedisDatabase :: Integer
    , incredibleRedisMaxConnections :: Int
    , incredibleRedisMaxIdleTimeout :: Integer
    , incredibleRedisPassword :: Maybe BS.ByteString
    , incredibleRedisRetryCount :: Int
    , incredibleWorkOrderTTL :: Integer
    , incredibleOrderBookTTL :: Integer
    , incredibleRedisUseTLS :: Bool
    }
  deriving (Show)

instance Toml.FromValue IncredibleRedisConfig where
  fromValue :: Toml.Value' l -> Toml.Matcher l IncredibleRedisConfig
  fromValue = Toml.parseTableFromValue $ do
    (host, port) <- lookupHostPort
    database <- Toml.reqKey "database"
    maxConnections <- Toml.reqKey "maxConnections"
    maxIdleTimeout <- Toml.reqKey "maxIdleTimeout"
    pass <- fmap TE.encodeUtf8 <$> Toml.optKey "password"
    useTLS <- fromMaybe False <$> Toml.optKey "tls"
    rcnt <- Toml.reqKey "retry_count"
    wottl <- Toml.reqKey "workorder_ttl"
    obttl <- Toml.reqKey "orderbook_ttl"
    pure $ IncredibleRedisConfig {
        incredibleRedisHostName = host
      , incredibleRedisPort = port
      , incredibleRedisDatabase = database
      , incredibleRedisMaxConnections = maxConnections
      , incredibleRedisMaxIdleTimeout = maxIdleTimeout
      , incredibleRedisPassword = pass
      , incredibleRedisRetryCount = rcnt
      , incredibleWorkOrderTTL = wottl
      , incredibleOrderBookTTL = obttl
      , incredibleRedisUseTLS = useTLS
      }
    where
      -- Lookup the port or unix socket
      lookupHostPort = do
        socket <- Toml.optKey "socket"
        case socket of
          Just sock -> pure ("", Redis.UnixSocket sock)
          Nothing -> do
            host <- Toml.reqKey "host"
            port <- Toml.reqKey "port"
            pure (host, Redis.PortNumber $ fromInteger port)
