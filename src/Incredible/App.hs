{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Incredible.App
( openMachineShop
, factoryRecall
, incredibleApp
, loadPuzzleMachine
, writePuzzleMachine
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Crypto.BCrypt as BCrypt
import qualified Data.Aeson as JS
import qualified Data.Cache.LRU.IO as LRU
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Encoding as Text
import           Incredible.API
import           Incredible.Config
import           Incredible.Data
import           Incredible.DataStore
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.Wai as WAI
import           Network.Wai.Middleware.AddHeaders
import           Network.Wai.Middleware.Cors
import           Network.Wai.Parse (defaultParseRequestBodyOptions, setMaxRequestFileSize)
import           Servant
import qualified Servant as Auth
import           Servant.Multipart (MultipartOptions(generalOptions) , defaultMultipartOptions, Mem)

-- | We load the puzzles from the machine, so structurally we have all the puzzles.
--   The puzzles might change over time though so we'll have to eject any Blueprints
--   that don't match the current puzzles. 
loadPuzzleMachine :: MonadIO m => FilePath -> m (MetaMachine Puzzle)
loadPuzzleMachine flnm = liftIO $ do
  ePzl <- JS.eitherDecodeFileStrict' flnm
  case ePzl of
    Left err -> fail $ "Could not load \""<>flnm<>"\": "<>err
    Right pzl -> pure pzl

-- | Write a generate machine to a file
writePuzzleMachine :: MonadIO m => FilePath -> MetaMachine Puzzle -> m ()
writePuzzleMachine flnm pzl = liftIO $ JS.encodeFile flnm pzl

-- | Looks at a datastore, and if the machine there has any out dated
--   puzzles or blueprints for outdated puzzles, updates them to the new spec.
factoryRecall :: forall s m . (Alternative m, MonadFail m, MonadIO m, IncredibleData s, MonadReader (MachineShop s) m) => MetaMachine Puzzle -> m ()
factoryRecall machineDesign = go
  where
    go = do
      cm <- vmMachine <$> getCurrentMachine
      remanufactured <- remanufacture machineDesign <$> traverse (either (pure . Left) (maybe (fail "Blueprint lookup failed!") (pure . Right) <=< getBlueprint) ) cm
      -- If we made no changes, we're done.
      when (cm /= remanufactured) $ do
        -- If we did an update, check if the machine hasn't changed and if it hasn't replace it and be done.
        -- If it has changed, leave it the same and try again.
        updated <- editCurrentMachine (\(VersionedMachine _ ngs) -> if ngs==cm then (True, remanufactured) else (False, ngs))
        if updated then pure () else go

openMachineShop :: (MonadIO m, IncredibleData s) => IncredibleConfig -> MetaMachine Puzzle -> (IncredibleConfig -> MetaMachine Puzzle -> IO (s, HashMap PuzzleID Puzzle)) -> m (MachineShop s)
openMachineShop ic pzlm store = liftIO $ do
  (s, pzlMap) <- store ic pzlm
  mbv  <- LRU.newAtomicLRU $ incredibleMachineByVersion   $ incredibleCacheConfig ic
  pbv  <- LRU.newAtomicLRU $ incrediblePuzzlesByVersion   $ incredibleCacheConfig ic
  bc   <- LRU.newAtomicLRU $ incredibleBlueprintByID      $ incredibleCacheConfig ic
  dc   <- LRU.newAtomicLRU $ incredibleDeltaCache         $ incredibleCacheConfig ic
  sc   <- LRU.newAtomicLRU $ incredibleSnapshotByID       $ incredibleCacheConfig ic
  b2pc <- LRU.newAtomicLRU $ incredibleBlueprint2Snapshot $ incredibleCacheConfig ic
  let ms = MachineShop
             { sdPuzzles       = pzlMap
             , sdMachineDesign = fmap puzzleID pzlm
             , sdModLogins     = incredibleMods ic
             , sdOrigins       = incredibleOrigins $ incredibleWebConfig ic
             , sdStore         = s
             , sdBaseUrl       = incredibleBaseUrl $ incredibleWebConfig ic
             , sdMachineByVersion      = mbv
             , sdReadyPuzzlesByVersion = pbv
             , sdBlueprintCache        = bc
             , sdDeltaCache            = dc
             , sdSnapshotCache         = sc
             , sdPuzzleLocCache        = cachePuzzleLoc $ fmap puzzleID pzlm
             , sdBlueprint2PuzzleCache = b2pc
             }
  (`runReaderT` ms) $ do
    factoryRecall pzlm
    cm <- getCurrentMachine
    forM_ (bpidsInMachine $ vmMachine cm) dequeueModeration
  pure ms

incredibleApp :: IncredibleData s => MachineShop s -> Application
incredibleApp icontext = addHeaders [("Vary", "Origin")] $ cors policy $ serveWithContextT (Proxy :: Proxy IncredibleAPI) context hoist server
  where
    size128k = 128*1024
    multipartOpts :: MultipartOptions Mem
    multipartOpts = (defaultMultipartOptions (Proxy :: Proxy Mem))
      { generalOptions = setMaxRequestFileSize size128k defaultParseRequestBodyOptions
      }
    -- Servant context
    context = multipartOpts :. authCheck :. EmptyContext
    -- Run handler with incredible context
    hoist = runIncredibleHandler icontext
    -- verify encrypted input = Scrypt.verifyPass' (Pass pass) (EncryptedPass encrypted)
    authCheck = Auth.BasicAuthCheck $ \auth -> do
      let user = Text.decodeUtf8 $ Auth.basicAuthUsername auth
      case HashMap.lookup user (sdModLogins icontext) of
        Just digest -> do
          let valid = BCrypt.validatePassword (Text.encodeUtf8 digest) (Auth.basicAuthPassword auth)
          pure $ if valid then Auth.Authorized user else Auth.BadPassword
        Nothing -> pure Auth.NoSuchUser
    policy req = Just CorsResourcePolicy
        { corsOrigins = if (lookup HTTP.hOrigin $ WAI.requestHeaders req) `elem` (fmap Just $ sdOrigins icontext) then Just (sdOrigins icontext, True) else Nothing
        , corsMethods = ["GET"]
        , corsRequestHeaders = ["authorization", "content-type", "X-WorkOrder"]
        , corsExposedHeaders = Just ["X-WorkOrder"]
        , corsMaxAge = Just $ 60*60*24 -- one day
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
      }
