{-# language ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Incredible.DataStore.Redis where

import Control.DeepSeq
import Control.Exception qualified as BE
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Aeson qualified as JS
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial qualified as S
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as BSL
import Data.Either
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Database.Redis (Redis)
import Database.Redis qualified as Redis
import Incredible.Config
import Incredible.Data
import Incredible.DataStore
--import Toml.Schema qualified as Toml
import Type.Reflection
import System.X509
import qualified Network.TLS as TLS
import Data.Default
import qualified Network.TLS.Extra.Cipher as Cipher

-- |
--   machinesKey :: Hash
--     - Watch machinesKey for transactions.
--     - HLen-1 machine version.
--     - machinesKey[version] for machine.
--     - HSETNX machines into it, shouldn't need a transaction since we're dense monotonic in IDs.
--     - Existamce of this key means the database is initialized.
--
--   blueprintsKey :: Hash
--     - blueprintsKey[blueprintID] for blueprints.
--     - HSETNX blueprints into it
--
--   snapshotsKey :: Hash
--     - snapshotsKey[blueprintID] for snapshots.
--     - HSET snapshots into it because we can change these in a critical situation
--
--   puzzlesKey :: Hash
--     - added at Init.
--
--   moderationKey<>puzzleID :: Set
--     - Data integrity here isn't so important. Just don't add anything that doesn't fit.
--
--   workKey<>WorkOrderID :: String
--     - volatile
--
--   widgetOrderKet<>WidgetOrder :: String
--     - Stores Int64s
--     - We incr and read
--     - Transactional integrity is, if anything, a drawback.

machinesKey :: BS.ByteString
machinesKey = "machines"

blueprintsKey :: BS.ByteString
blueprintsKey = "blueprints"

snapshotsKey :: BS.ByteString
snapshotsKey = "snapshots"

puzzlesKey :: BS.ByteString
puzzlesKey = "puzzles"

moderationKey :: PuzzleID -> BS.ByteString
moderationKey pid = "moderation:" <> encodeUUID pid

workKey :: WorkOrderID -> BS.ByteString
workKey (WorkOrderID wid) = "work:" <> encodeUUID wid

widgetOrderKey :: WidgetSignature -> BS.ByteString
widgetOrderKey ws = "widget_"<>runPutS (S.serialize ws)

data IncredibleRedisStore
  = IncredibleRedisStore
    { incredibleRedisConnection :: Redis.Connection
    , retryCount :: Int
    , workOrderTermSeconds :: Integer
    , widgetOrderBookTermSeconds :: Integer
    }

logger :: MonadIO m => Bool -> String -> m ()
logger False = void . pure
logger True  = liftIO . print

initConnectInfo :: MonadIO m => IncredibleRedisConfig -> m Redis.ConnectInfo
initConnectInfo cfg = do
  certStore <- liftIO $ getSystemCertificateStore
  let tlsParams =
        if incredibleRedisUseTLS cfg
          then Just $ (TLS.defaultParamsClient (incredibleRedisHostName cfg) "") {
                  TLS.clientSupported = def {TLS.supportedCiphers = Cipher.ciphersuite_default }
                , TLS.clientShared = def { TLS.sharedCAStore = certStore }
                }
          else Nothing
  pure $ Redis.defaultConnectInfo {
            Redis.connectHost = incredibleRedisHostName cfg
          , Redis.connectPort = incredibleRedisPort cfg
          , Redis.connectAuth = incredibleRedisPassword cfg
          , Redis.connectDatabase = incredibleRedisDatabase cfg
          , Redis.connectMaxConnections = incredibleRedisMaxConnections cfg
          , Redis.connectMaxIdleTime = fromInteger $ incredibleRedisMaxIdleTimeout cfg
          , Redis.connectTLSParams = tlsParams
        }
initIncredibleState :: MonadIO m => IncredibleConfig -> MetaMachine Puzzle -> m (IncredibleRedisStore, HashMap PuzzleID Puzzle)
initIncredibleState (IncredibleConfig {incredibleRedis = Nothing}) _ = liftIO $ fail "No Redis config"
initIncredibleState (IncredibleConfig {incredibleRedis = Just cfg}) ipzl = do
  conninfo <- initConnectInfo cfg
  conn <- liftIO $ Redis.checkedConnect $ conninfo
  let is = IncredibleRedisStore conn retries wottl wobttl
  didInit <- raisingException is $ liftRedis $
    Redis.hsetnx machinesKey (encodeVersion 0) $ encodeMachine (Left . puzzleID <$> ipzl)
  when didInit $ logger False ("Initialized DataStore"::String)
  let pzlList = map (\p -> (puzzleID p, p)) $ toList ipzl
  -- Get all puzzles ever
  pzlRedisList <- do
    forM_ pzlList $ \(pid, p) -> raisingException is $ liftRedis $
      Redis.hsetnx puzzlesKey (encodeUUID pid) $ encodePuzzle p
    rpzls <- raisingException is $ liftRedis $ Redis.hgetall puzzlesKey
    fmap catMaybes . forM rpzls $ \(bpid, bpzl) -> runMaybeT $ do
      pid <- maybe (logger True "couldn't decode PuzzleID" >> mzero) pure $ decodeUUID bpid
      p <- maybe (logger True ("couldn't decode Puzzle: "<>show bpid) >> mzero) pure $ decodePuzzle bpzl
      pure (pid, p)
  pure (is, HashMap.fromList $ pzlList <> pzlRedisList)
  where
    retries = incredibleRedisRetryCount cfg
    wottl = incredibleWorkOrderTTL cfg
    wobttl = incredibleOrderBookTTL cfg

raisingException :: (MonadIO m, NFData a) => IncredibleRedisStore -> ExceptT RedisError Redis a -> m a
raisingException store act = fmap force $
  (either BE.throw pure <=< (liftIO . Redis.runRedis (incredibleRedisConnection store))) $ runExceptT act

data RedisError =
    RedisReplyError Redis.Reply
  | RedisTxError String
  | RedisTxAborted String
  | RedisRetryLimitReached String
  | RedisGetNotFound BS.ByteString
  | RedisHGetNotFound BS.ByteString BS.ByteString
  | RedisMachineDecodeError String
  | RedisVersionDecodeError String
  | RedisBlueprintDecodeError String
  | RedisSnapshotDecodeError String
  | RedisUUIDDecodeError BS.ByteString
  | RedisMachineVersionNotFound MachineVersion
  | RedisIntegrityError String
  deriving (Show, Eq, Typeable)

instance BE.Exception RedisError

retryTransaction :: Int -> ExceptT RedisError Redis a -> ExceptT RedisError Redis a
retryTransaction count tx = go count
  where
    go c = catchError tx $ \case
            RedisTxAborted source -> if c > 0 then go (c - 1) else BE.throw $ RedisRetryLimitReached source
            err -> BE.throw err

encodeUUID :: UUID -> BS.ByteString
encodeUUID = BSL.toStrict . UUID.toByteString

decodeUUID :: BS.ByteString -> Maybe UUID
decodeUUID = UUID.fromByteString . BSL.fromStrict

encodeVersion :: MachineVersion -> BS.ByteString
encodeVersion = Char8.pack . show -- TODO this is not great

decodePuzzle :: BS.ByteString -> Maybe Puzzle
decodePuzzle contents =
  case JS.eitherDecodeStrict contents of
    Left _ -> Nothing
    Right version -> Just version

encodeWorkOrder :: WorkOrder -> Char8.ByteString
encodeWorkOrder = BSL.toStrict . JS.encode

decodeWorkOrder :: BS.ByteString -> Maybe WorkOrder
decodeWorkOrder contents =
  case JS.eitherDecodeStrict contents of
    Left _ -> Nothing
    Right wo -> pure wo

encodePuzzle :: Puzzle -> Char8.ByteString
encodePuzzle = BSL.toStrict . JS.encode

decodeBlueprint :: BS.ByteString -> ExceptT RedisError Redis Blueprint
decodeBlueprint contents =
  case JS.eitherDecodeStrict contents of
    Left err -> throwError $ RedisBlueprintDecodeError err
    Right bp -> pure bp

encodeBlueprint :: Blueprint -> Char8.ByteString
encodeBlueprint = BSL.toStrict . JS.encode

decodeSnapshot :: BS.ByteString -> ExceptT RedisError Redis Snapshot
decodeSnapshot contents =
  case JS.eitherDecodeStrict contents of
    Left err -> throwError $ RedisSnapshotDecodeError err
    Right ss -> pure ss

encodeSnapshot :: Snapshot -> Char8.ByteString
encodeSnapshot = BSL.toStrict . JS.encode

encodeMachine :: GameState -> Char8.ByteString
encodeMachine = BSL.toStrict . JS.encode

getMachineVersion :: (Functor f, Redis.RedisCtx m f) => MachineVersion -> m (f (Either RedisError GameState))
getMachineVersion mv = do
  machineJSON <- Redis.hget machinesKey (encodeVersion mv)
  pure $ maybe (Left $ RedisMachineVersionNotFound mv) (either (Left . RedisMachineDecodeError) Right . JS.eitherDecodeStrict) <$> machineJSON

execTrans :: String -> (forall m f . (Functor f, Redis.RedisCtx m f) => m (f (Either RedisError a))) -> ExceptT RedisError Redis a
execTrans source txn = do
  txr <- lift $ Redis.multiExec txn
  case txr of
    Redis.TxSuccess (Left err) -> throwError err
    Redis.TxSuccess (Right r) -> pure r
    Redis.TxAborted -> throwError $ RedisTxAborted source
    Redis.TxError err -> throwError $ RedisTxError err

liftRedis :: Redis (Either Redis.Reply a) -> ExceptT RedisError Redis a
liftRedis act = do
  resp <- lift act
  case resp of
    (Left rep) -> throwError $ RedisReplyError rep
    (Right r)  -> pure r

instance IncredibleData IncredibleRedisStore where
  getCurrentMachine' conn = raisingException conn $ do
      retryTransaction (retryCount conn) $ do
        Redis.Ok <- liftRedis $ Redis.watch [machinesKey]
        numMachines <- liftRedis $ Redis.hlen machinesKey
        let curMachineVersion = numMachines-1
        VersionedMachine curMachineVersion <$> execTrans "getCurrentMachine" (getMachineVersion curMachineVersion)
  {-# INLINE getCurrentMachine' #-}

  getMachine' conn version = raisingException conn $ do
    handleError (\case {RedisMachineVersionNotFound _ -> pure Nothing; err -> throwError err}) $ do
      fmap Just $ liftEither =<< liftRedis (getMachineVersion version)
  {-# INLINE getMachine' #-}

  editCurrentMachine' conn f = raisingException conn $ do
    retryTransaction (retryCount conn) $ do
      Redis.Ok <- liftRedis $ Redis.watch [machinesKey]
      numMachines <- liftRedis $ Redis.hlen machinesKey
      let curMachineVersion = numMachines-1
      let newMachineVersion = succ curMachineVersion
      machine <- liftEither =<< liftRedis (getMachineVersion curMachineVersion)
      let (r, newMachine) = f (VersionedMachine curMachineVersion machine)
      when (newMachine /= machine) $ do
        commited <- execTrans "editCurrentMachine" $ fmap (fmap Right) $ Redis.hsetnx machinesKey (encodeVersion newMachineVersion) $ encodeMachine newMachine
        unless commited $ throwError $ RedisTxAborted "Raced editing machine"
      pure r
  {-# INLINE editCurrentMachine' #-}

  addBlueprint' conn blueprint = raisingException conn $ void $ liftRedis $
    Redis.hsetnx blueprintsKey (encodeUUID $ blueprintID blueprint) $ encodeBlueprint blueprint
  {-# INLINE addBlueprint' #-}

  getBlueprint' conn bid = raisingException conn $ do
    liftRedis (Redis.hget blueprintsKey (encodeUUID bid)) >>= maybe (pure Nothing) (fmap Just . decodeBlueprint)
  {-# INLINE getBlueprint' #-}

  addSnapshot' conn blueprint snapshot = raisingException conn $ void $ liftRedis $
    Redis.hset snapshotsKey (encodeUUID blueprint) $ encodeSnapshot snapshot
  {-# INLINE addSnapshot' #-}

  getSnapshot' conn bid = raisingException conn $ do
    liftRedis (Redis.hget snapshotsKey (encodeUUID bid)) >>= maybe (pure Nothing) (fmap Just . decodeSnapshot)
  {-# INLINE getSnapshot' #-}

  queueModeration' conn pid bid = raisingException conn $ do
    void $ liftRedis $ Redis.sadd (moderationKey pid) [encodeUUID bid]
  {-# INLINE queueModeration' #-}

  dequeueModeration' conn pid bid = raisingException conn $ do
    void $ liftRedis $ Redis.srem (moderationKey pid) [encodeUUID bid]
  {-# INLINE dequeueModeration' #-}

  listModerationQueue' conn pid = raisingException conn $ do
    fmap (mapMaybe decodeUUID) $ liftRedis $ Redis.smembers (moderationKey pid)
  {-# INLINE listModerationQueue' #-}

  modQueueLength' conn pids = raisingException conn $ do
    liftRedis $ fmap (Right . rights) . forM pids $ \pid -> fmap (pid,) <$> Redis.scard (moderationKey pid)
  {-# INLINE modQueueLength' #-}

  addWorkOrder' conn wid w = raisingException conn $ do
    let wokey = workKey wid
    void $ execTrans "addWorkOrder" $ do
      void $ Redis.set wokey (encodeWorkOrder w)
      fmap Right <$> Redis.expire wokey (workOrderTermSeconds conn)
  {-# INLINE addWorkOrder' #-}

  pullWorkOrder' conn wid = raisingException conn $ do
    let wokey = workKey wid
    execTrans "pullWorkOrder" $ do
      mwo <- (fmap (Right . join . fmap decodeWorkOrder)) <$> Redis.get wokey
      void $ Redis.del [wokey]
      pure mwo
  {-# INLINE pullWorkOrder' #-}

  widgetOrderBook' conn ws = raisingException conn $ do
    let woKey = widgetOrderKey ws
    execTrans "addWorkOrder" $ do
      wob <- fmap fromInteger <$> Redis.incr woKey
      void $ Redis.expire woKey (widgetOrderBookTermSeconds conn)
      pure $ fmap Right wob
  {-# INLINE widgetOrderBook' #-}
