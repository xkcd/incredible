{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Incredible.DataStore
 ( IncredibleData(..)
 , HasDataStore(..)
 , getCurrentMachine
 , getMachine
 , editCurrentMachine
 , getBlueprint, addSnapshot, getSnapshot
 , queueModeration
 , dequeueModeration
 , listModerationQueue
 , modQueueLength
 , addWorkOrder
 , pullWorkOrder
 , widgetOrderBook
 , lruFetch
 ) where

import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Cache.LRU.IO (AtomicLRU)
import qualified Data.Cache.LRU.IO as LRU
import           Data.Foldable
import           Data.Int
import           Data.Traversable
import           Incredible.Data

class IncredibleData s where

  getCurrentMachine' :: MonadIO m => s -> m VersionedGameState
  getMachine' :: MonadIO m => s -> MachineVersion -> m (Maybe GameState)
  -- | Must make no edit if the machine is the same as the current machine.
  editCurrentMachine' :: (MonadIO m, NFData a) => s -> (VersionedGameState -> (a, GameState)) -> m a
  addBlueprint' :: MonadIO m => s -> Blueprint -> m ()
  getBlueprint' :: MonadIO m => s -> BlueprintID -> m (Maybe Blueprint)

  addSnapshot' :: MonadIO m => s -> BlueprintID -> Snapshot -> m ()
  getSnapshot' :: MonadIO m => s -> BlueprintID -> m (Maybe Snapshot)

  queueModeration' :: MonadIO m => s -> PuzzleID -> BlueprintID -> m ()
  dequeueModeration' :: MonadIO m => s -> PuzzleID -> BlueprintID -> m ()
  listModerationQueue' :: MonadIO m => s -> PuzzleID -> m [BlueprintID]
  modQueueLength' ::  MonadIO m => s -> [PuzzleID] -> m [(PuzzleID, Integer)]
  modQueueLength' s pids = forM pids $ \pid -> fmap ((pid,) . toInteger . length) $ listModerationQueue' s pid

  addWorkOrder' :: MonadIO m => s -> WorkOrderID -> WorkOrder -> m ()
  -- | Work orders are single use.
  pullWorkOrder' :: MonadIO m => s -> WorkOrderID -> m (Maybe WorkOrder)

  -- | Records the use of a widget and provides an estimate of the number of times it has been used.
  widgetOrderBook' :: MonadIO m => s -> WidgetSignature -> m Int64

--  sharedData :: MonadIO m => s -> (ServerSharedState -> ServerSharedState) -> m ServerSharedState

lruFetch :: (Ord key, MonadIO m, NFData val) => (key -> m (Maybe val)) -> key -> AtomicLRU key val -> m (Maybe val)
lruFetch fetcher k lru = do
   mr <- liftIO $ LRU.lookup k lru
   case mr of
    val@(Just _) -> pure val
    Nothing -> do
      newVal <- fetcher k
      -- Make sure everything in cache is fully evaluated.
      deepseq newVal $ liftIO $ traverse_ (\n -> LRU.insert k n lru) newVal
      pure newVal
{-# INLINE lruFetch #-}

class IncredibleData s => HasDataStore r s | r -> s where
  getStore :: r -> s
  getMachineCache :: r ->  AtomicLRU MachineVersion GameState
  getBlueprintCache :: r -> AtomicLRU BlueprintID Blueprint
  getSnapshotCache :: r -> AtomicLRU BlueprintID Snapshot

instance IncredibleData s => HasDataStore (MachineShop s) s where
  getStore = sdStore
  getMachineCache = sdMachineByVersion
  getBlueprintCache = sdBlueprintCache
  getSnapshotCache = sdSnapshotCache

getCurrentMachine :: (MonadIO m, IncredibleData s, HasDataStore r s, MonadReader r m) => m VersionedGameState
getCurrentMachine = getCurrentMachine' =<< asks getStore
{-# INLINE getCurrentMachine #-}

getMachine :: (MonadIO m, IncredibleData s, HasDataStore r s, MonadReader r m) => MachineVersion -> m (Maybe GameState)
getMachine mv = lruFetch (\k -> (`getMachine'` k) =<< asks getStore) mv =<< asks getMachineCache
{-# INLINE getMachine #-}

editCurrentMachine :: (MonadIO m, IncredibleData s, HasDataStore r s, MonadReader r m, NFData a) => (VersionedGameState -> (a, GameState)) -> m a
editCurrentMachine act = (`editCurrentMachine'` act) =<< asks getStore
{-# INLINE editCurrentMachine #-}

-- | IMPLICITE TO queueModeration
addBlueprint :: (MonadIO m, IncredibleData s, HasDataStore r s, MonadReader r m) => Blueprint -> m ()
addBlueprint b = (`addBlueprint'` b) =<< asks getStore
{-# INLINE addBlueprint #-}

getBlueprint :: (MonadIO m, IncredibleData s, HasDataStore r s, MonadReader r m) => BlueprintID -> m (Maybe Blueprint)
getBlueprint bid = lruFetch (\k -> (`getBlueprint'` k) =<< asks getStore) bid =<< asks getBlueprintCache
{-# INLINE getBlueprint #-}

addSnapshot :: (MonadIO m, IncredibleData s, HasDataStore r s, MonadReader r m) => BlueprintID -> Snapshot -> m ()
addSnapshot b s = (\d -> addSnapshot' d b s) =<< asks getStore
{-# INLINE addSnapshot #-}

getSnapshot :: (MonadIO m, IncredibleData s, HasDataStore r s, MonadReader r m) => BlueprintID -> m (Maybe Snapshot)
getSnapshot bid = lruFetch (\k -> (`getSnapshot'` k) =<< asks getStore) bid =<< asks getSnapshotCache
{-# INLINE getSnapshot #-}

queueModeration :: (MonadIO m, IncredibleData s, HasDataStore r s, MonadReader r m) => Blueprint -> m ()
queueModeration bp = do
  addBlueprint bp
  (\s -> queueModeration' s (bPuzzleID bp) (blueprintID bp)) =<< asks getStore
{-# INLINE queueModeration #-}

dequeueModeration :: (MonadIO m, IncredibleData s, HasDataStore r s, MonadReader r m) => BlueprintID -> m ()
dequeueModeration bpid = do
  maybe (pure ()) (\bp -> (\s -> dequeueModeration' s (bPuzzleID bp) bpid) =<< asks getStore) =<< getBlueprint bpid
{-# INLINE dequeueModeration #-}

listModerationQueue :: (MonadIO m, IncredibleData s, HasDataStore r s, MonadReader r m) => PuzzleID -> m [BlueprintID]
listModerationQueue pid = (`listModerationQueue'` pid) =<< asks getStore
{-# INLINE listModerationQueue #-}

modQueueLength :: (MonadIO m, IncredibleData s, HasDataStore r s, MonadReader r m) => [PuzzleID] -> m [(PuzzleID, Integer)]
modQueueLength pid = (`modQueueLength'` pid) =<< asks getStore
{-# INLINE modQueueLength #-}

addWorkOrder :: (MonadIO m, IncredibleData s, HasDataStore r s, MonadReader r m) => WorkOrderID -> WorkOrder -> m ()
addWorkOrder wid w = (\s -> addWorkOrder' s wid w) =<< asks getStore
{-# INLINE addWorkOrder #-}

pullWorkOrder :: (MonadIO m, IncredibleData s, HasDataStore r s, MonadReader r m) => WorkOrderID -> m (Maybe WorkOrder)
pullWorkOrder wid = (`pullWorkOrder'` wid) =<< asks getStore
{-# INLINE pullWorkOrder #-}

widgetOrderBook :: (MonadIO m, IncredibleData s, HasDataStore r s, MonadReader r m) => WidgetSignature -> m Int64
widgetOrderBook ws = (`widgetOrderBook'` ws) =<< asks getStore
{-# INLINE widgetOrderBook #-}
