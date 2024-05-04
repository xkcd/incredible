{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Incredible.API where

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.Array as Array
import           Data.Cache.LRU.IO (AtomicLRU)
import qualified Data.Cache.LRU.IO as LRU
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Ix as Ix
import           Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           Incredible.AntiEvil
import           Incredible.Data
import           Incredible.DataStore
import           Servant
import           System.Random
import           System.IO

type CacheHeader = Header "Cache-Control" String
type WorkOrderHeader = Header "X-WorkOrder" T.Text

type BlueprintApi = "blueprint" :> "file" :> WorkOrderHeader :> ReqBody '[JSON] Blueprint :> Post '[JSON] (BlueprintID, (X, Y))

-- Only caches if it has a snapshot
type FolioApi = "folio" :> Capture "blueprintid" BlueprintID :> Get '[JSON] (Headers '[CacheHeader] Folio)

-- | Redirect to current Machine
--   If just the current version, Blueprint IDs or Full, want If-Modified

type MachineApi = "machine" :> (
                         "current" :> (
                                 Get '[JSON] (Headers '[CacheHeader] (VersionedMachine (Maybe BlueprintID))) -- ^ Redirects to ve[((X, Y), rsioned version which is cachable.
                            :<|> "version" :> Get '[JSON] (Headers '[CacheHeader] MachineVersion))
                    :<|> Capture "version" MachineVersion :> Get '[JSON] (Headers '[CacheHeader] (VersionedMachine (Maybe BlueprintID)))
                  )

type DeltaApi =
       "machine" :> "delta" :> (
               Capture "startVersion" MachineVersion :> "current" :> Get '[JSON] (Headers '[CacheHeader] (MachineVersion, MachineUpdates BlueprintID)) -- ^ Redirects to two version version which is cachable.
          :<|> Capture "startVersion" MachineVersion :> Capture "endVersion" MachineVersion :> Get '[JSON] (Headers '[CacheHeader] (MachineVersion, MachineUpdates BlueprintID))
        )

type PuzzleAPI = "puzzle" :> Get '[JSON] (Headers '[CacheHeader, WorkOrderHeader] (HashMap PuzzleID Puzzle))

-- | Upload a machine
-- type "submit"

-- Moderation
type ModAPI = "moderate" :> BasicAuth "incredible mod api" UserName :> (
         "puzzle" :> Capture "puzzleid" PuzzleID :> "blueprintid" :> Get '[JSON] (Headers '[CacheHeader] [BlueprintID]) -- JSONL?
    :<|> "puzzle" :> Capture "puzzleid" PuzzleID :> "blueprint" :> Get '[JSON] (Headers '[CacheHeader] [(BlueprintID, Blueprint)]) -- JSONL?
    :<|> "puzzle" :> Capture "puzzleid" PuzzleID :> Get '[JSON] (Headers '[CacheHeader] Puzzle)
    :<|> "puzzle" :> Capture "puzzleid" PuzzleID :> "reissue" :> PostNoContent
    :<|> "build"  :> Capture "X" X :> Capture "Y" Y :> ReqBody '[JSON] InspectionReport :> PostNoContent
    :<|> "burn"   :> Capture "blueprintid" BlueprintID :> PostNoContent -- Just removed from the mod queue, delete if never added to any GameState?
    :<|> "machine" :> "current" :> Get '[JSON] (Headers '[CacheHeader] (VersionedMachine ModData))
  )

type IncredibleAPI =
         BlueprintApi
    :<|> FolioApi
    :<|> MachineApi
    :<|> DeltaApi
    :<|> PuzzleAPI
    :<|> ModAPI

incredibleAPI :: Proxy IncredibleAPI
incredibleAPI = Proxy

server :: forall s. IncredibleData s => ServerT IncredibleAPI (IncredibleHandler s)
server =
       blueprintHandlers
  :<|> folioHandlers
  :<|> machineHandlers
  :<|> deltaHandlers
  :<|> puzzleHandlers
  :<|> modHandlers
  where
    blueprintHandlers = handleAddBlueprint
    folioHandlers = handleGetFolio
    machineHandlers =
           (handleMachineCurrentBlueprintID :<|> handleCurrentVersion) :<|> handleMachineBlueprintID
    deltaHandlers :: IncredibleData s => ServerT DeltaApi (IncredibleHandler s)
    deltaHandlers = handleGetCurrentDelta :<|> handleGetDelta
    puzzleHandlers = handleGetPuzzles
    modHandlers user = handlePuzzleBlueprintID user :<|> handlePuzzleBlueprint user :<|> handlePuzzle user :<|> handleReissue user :<|> handleBuild user :<|> handleBurn user :<|> handleModMachine user

newtype IncredibleHandler s a = IncredibleHandler { unIncredibleHandler :: ReaderT (MachineShop s) Handler a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (MachineShop s), MonadError ServerError)

runIncredibleHandler :: MachineShop s -> IncredibleHandler s a -> Handler a
runIncredibleHandler config = flip runReaderT config . unIncredibleHandler

cacheForever :: AddHeader "Cache-Control" String orig new => IncredibleHandler s orig -> IncredibleHandler s new
cacheForever hdlr = addHeader "max-age=86400" <$> hdlr

noCache :: AddHeader "Cache-Control" String orig new => IncredibleHandler s orig -> IncredibleHandler s new
noCache hdlr = addHeader "no-store" <$> hdlr

handleGetFolio :: IncredibleData s => BlueprintID -> IncredibleHandler s (Headers '[CacheHeader] Folio)
handleGetFolio bpID = do
  blueprint <- maybe blueprint404 pure =<< getBlueprint bpID
  puzzle    <- maybe (throwError $ err500 { errBody = "Puzzle not found" }) pure =<< asks (HashMap.lookup (bPuzzleID blueprint) . sdPuzzles)
  msnapshot <- getSnapshot bpID
  (if isJust msnapshot then cacheForever else noCache) $ pure $
    Folio puzzle blueprint msnapshot

handleAddBlueprint :: IncredibleData s => Maybe T.Text -> Blueprint -> IncredibleHandler s (BlueprintID, (X, Y))
handleAddBlueprint woidt bp' = do
  now <- liftIO $ getCurrentTime
  let bp = bp' { bSubmittedAt = Just now }
  doAdd <- checkWorkOrder woidt bp
  pzlLocCache <- asks sdPuzzleLocCache
  let pzlLoc = HashMap.findWithDefault mempty (bPuzzleID bp) pzlLocCache
  liftIO $ print (doAdd, bp)
  liftIO $ print pzlLoc
  liftIO $ hFlush stdout
  if doAdd
    then queueModeration bp
    else pure () -- TODO: Sleep for average queueModeration time
  -- Sleep 50 to 150ms to make timing attacks less trivial.
  liftIO $ randomRIO (50*1000, 150*1000) >>= threadDelay
  pure (blueprintID bp, fromMaybe (0,0) $ fmap fst $ UV.uncons pzlLoc)

handleMachineCurrentBlueprintID :: IncredibleData s => IncredibleHandler s (Headers '[CacheHeader] (VersionedMachine (Maybe BlueprintID)))
handleMachineCurrentBlueprintID = noCache $ do
  VersionedMachine v _ <- getCurrentMachine
  burl <- asks sdBaseUrl
  throwError err307 { errHeaders = [("Location", T.encodeUtf8 $ burl<>"machine/"<>T.pack (show v))]}

handleMachineBlueprintID :: IncredibleData s => MachineVersion -> IncredibleHandler s (Headers '[CacheHeader] (VersionedMachine (Maybe BlueprintID)))
handleMachineBlueprintID mv = cacheForever $ do
  VersionedMachine mv . fmap (either (const Nothing) Just) <$> lookupMachine mv

handleCurrentVersion :: IncredibleData s => IncredibleHandler s (Headers '[CacheHeader] MachineVersion)
handleCurrentVersion = noCache $ vmVersion <$> getCurrentMachine

handleGetDelta :: IncredibleData s => MachineVersion -> MachineVersion -> IncredibleHandler s (Headers '[CacheHeader]  (MachineVersion, MachineUpdates BlueprintID))
handleGetDelta startVersion endVersion = cacheForever $ fmap (endVersion,) $
  (lruGenerate (machineDeltaHandler startVersion endVersion) (startVersion, endVersion)) =<< asks sdDeltaCache

handleGetCurrentDelta :: IncredibleData s => MachineVersion -> IncredibleHandler s (Headers '[CacheHeader] (MachineVersion, MachineUpdates BlueprintID))
handleGetCurrentDelta startVersion = noCache $ do
  VersionedMachine v _ <- getCurrentMachine
  burl <- asks sdBaseUrl
  throwError err307 { errHeaders = [("Location", T.encodeUtf8 $ burl<>"machine/delta/"<>T.pack (show startVersion)<>"/"<>T.pack (show v))]}

lookupMachine :: IncredibleData s => MachineVersion -> IncredibleHandler s GameState
lookupMachine version = maybe machineNotFound pure =<< getMachine version
  where
    machineNotFound = throwError $ err404 { errBody = "Machine not found" }

machineDeltaHandler :: IncredibleData s =>  MachineVersion -> MachineVersion -> IncredibleHandler s (MachineUpdates BlueprintID)
machineDeltaHandler startVersion endVersion = do
  start <- lookupMachine startVersion
  end   <- lookupMachine endVersion
  let delta = stripNothings $ deltaMachine (toBlueprintID <$> start) (toBlueprintID <$> end)
  pure delta
  where
    stripNothings :: MachineUpdates (Maybe a) -> MachineUpdates a
    stripNothings d = d { muConstruction = V.mapMaybe (\(i, ma) -> (i,) <$> ma) $ muConstruction d }
    toBlueprintID :: Either a BlueprintID -> Maybe BlueprintID
    toBlueprintID = either (const Nothing) Just

lruGenerate :: (Ord key, MonadIO m, NFData val) => m val -> key -> AtomicLRU key val -> m val
lruGenerate gen k lru = do
   mr <- liftIO $ LRU.lookup k lru
   case mr of
    (Just val) -> pure val
    Nothing -> do
      newVal <- gen
      newVal `deepseq` liftIO (LRU.insert k newVal lru)
      pure newVal

readyPuzzles :: IncredibleData s => VersionedGameState -> IncredibleHandler s (HashMap PuzzleID Puzzle)
readyPuzzles (VersionedMachine v m) = do
  lruGenerate (asks (HashMap.fromList . (`findReadEnoughPuzzles` m) . sdPuzzles)) v =<< asks sdReadyPuzzlesByVersion

findReadEnoughPuzzles :: HashMap PuzzleID Puzzle -> GameState -> [(PuzzleID, Puzzle)]
findReadEnoughPuzzles pzls m = --findReadyPuzzles pzls m
  let rdy = prioPuzzles <> findReadyPuzzles pzls m
      fpw  = firstPuzzles pzls m
      prioPuzzles = mapMaybe (\pid -> (pid,) <$> HashMap.lookup pid pzls) $ V.toList $ mmPrioPuzzles m
  in Set.toList $ Set.fromList (rdy <> (take 50 fpw))
  --if not (null rdy) then rdy else take (10-length rdy) fpw

handleGetPuzzles :: IncredibleData s => IncredibleHandler s (Headers '[CacheHeader, WorkOrderHeader] (HashMap PuzzleID Puzzle))
handleGetPuzzles = noCache $ do
  cm <- getCurrentMachine
  pzlmp <- readyPuzzles cm
  woid <- issueWorkOrder $ fmap fst $ HashMap.toList pzlmp
  pure $ addHeader woid $ pzlmp

handlePuzzleBlueprintID :: IncredibleData s => UserName -> PuzzleID -> IncredibleHandler s (Headers '[CacheHeader] [BlueprintID])
handlePuzzleBlueprintID _user = noCache . listModerationQueue -- TODO limit number of results?

handlePuzzleBlueprint :: IncredibleData s => UserName -> PuzzleID -> IncredibleHandler s (Headers '[CacheHeader] [(BlueprintID, Blueprint)])
handlePuzzleBlueprint _user pid = noCache $ do
  bids <- listModerationQueue pid
  catMaybes <$> traverse (\bpid -> (fmap (bpid,)) <$> getBlueprint bpid) bids -- TODO this silently drops errors, should it?

handleReissue :: IncredibleData s => UserName -> PuzzleID -> IncredibleHandler s NoContent
handleReissue _user pid = do
  pzlMap <- asks sdPuzzleLocCache
  editCurrentMachine (\(VersionedMachine _ mm) -> do
    -- Only prioritize it if it is in the current machine, also clear out old prioritized ones.
    ((), mm {mmPrioPuzzles = V.filter (`HashMap.member` pzlMap) $ (mmPrioPuzzles mm) `V.snoc` pid}))
  pure NoContent

handlePuzzle :: IncredibleData s => UserName -> PuzzleID -> IncredibleHandler s (Headers '[CacheHeader] Puzzle)
handlePuzzle _user pid = noCache $ do
  maybe (throwError $ err404 { errBody = "Puzzle not found" }) pure =<< asks (HashMap.lookup pid . sdPuzzles)

blueprint404 :: IncredibleHandler s a
blueprint404  = throwError $ err404 { errBody = "Blueprint not found" }

handleBuild :: IncredibleData s => UserName -> X -> Y -> InspectionReport -> IncredibleHandler s NoContent
handleBuild _user x y (InspectionReport bpid snap) = do
  design <- mmGrid <$> asks sdMachineDesign
  let loc = (x, y)
  unless (Ix.inRange (Array.bounds design) loc) $ throwError $ err400 { errBody = "Not in bounds" }
  bp <- maybe blueprint404 pure =<< getBlueprint bpid
  () <- if (design Array.! (x, y))==bPuzzleID bp
        then do
          dequeueModeration bpid
          addSnapshot bpid snap
          wasThere <- editCurrentMachine (\(VersionedMachine _ mm) ->
                        if bpid `elem` (bpidsInMachine mm)
                          then (True, mm)
                          else (False, mm { mmPrioPuzzles = V.filter (/=(bPuzzleID bp)) $ mmPrioPuzzles mm, mmGrid = mmGrid mm Array.// [((x,y), Right bpid)]})
                       )
          when wasThere $ throwError $ err409 { errBody = "Blueprint already in use in the machine." }
        else throwError $ err400 { errBody = "Blueprint not a solution for that puzzle" }
  pure NoContent

handleBurn :: IncredibleData s => UserName -> BlueprintID -> IncredibleHandler s NoContent
handleBurn _user bpid = dequeueModeration bpid >> pure NoContent

handleModMachine :: IncredibleData s => UserName -> IncredibleHandler s (Headers '[CacheHeader] (VersionedMachine ModData))
handleModMachine _user = noCache $ do
  cm <- getCurrentMachine
  worthModerating <- HashMap.keys <$> readyPuzzles cm
  modLengthMap <- HashMap.fromList <$> modQueueLength worthModerating
  VersionedMachine (vmVersion cm) <$> (`translateMachine` vmMachine cm) (\_xy tileContent -> do
    let mbpid = either (const Nothing) Just tileContent
    pid  <- either (pure)
                   (\bpid -> maybe blueprint404 pure =<< fetchBPPuzzleID bpid)
                   tileContent
    pure $ ModData mbpid pid (HashMap.lookup pid modLengthMap)
    )

fetchBPPuzzleID :: IncredibleData s => BlueprintID -> IncredibleHandler s (Maybe PuzzleID)
fetchBPPuzzleID bpid = lruFetch (fmap (fmap bPuzzleID) . getBlueprint) bpid =<< asks sdBlueprint2PuzzleCache
