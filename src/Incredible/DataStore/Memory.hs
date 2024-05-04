{-# language ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
module Incredible.DataStore.Memory where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified  as HashMap
import Data.Int
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Incredible.Data
import Incredible.DataStore

data IncredibleState = IncredibleState {
  machines :: Map MachineVersion GameState
, blueprints :: Map BlueprintID Blueprint
, snapshots :: Map BlueprintID Snapshot
, moderation :: Map PuzzleID (Set.Set BlueprintID)
, workorders :: Map WorkOrderID WorkOrder
, widgetOrders :: Map WidgetSignature Int64
}

newtype IncredibleStore = IncredibleStore {
  getIncredibleStore :: TVar IncredibleState
}

initIncredibleState :: MonadIO m
                    => a
                    -> MetaMachine Puzzle
                    -> m (IncredibleStore, HashMap PuzzleID Puzzle)
initIncredibleState _ machine = liftIO $ do
  let state = IncredibleState (Map.singleton 0 $ fmap (Left . puzzleID) machine) mempty mempty mempty mempty mempty
  store <- newTVarIO state
  pure $ (IncredibleStore store, HashMap.fromList $ map (\p -> (puzzleID p, p)) $ toList machine)

instance IncredibleData IncredibleStore where

  getCurrentMachine' store = liftIO $ do
    state <- readTVarIO $ getIncredibleStore store
    -- Safe because the map is forced to contain the initial machine.
    maybe (fail "initial machine missing from map") (\(v, m) -> pure (VersionedMachine v m)) $ Map.lookupMax $ machines state
  getMachine' store version = liftIO $ do
    state <- readTVarIO $ getIncredibleStore store
    pure $ Map.lookup version $ machines state
  editCurrentMachine' store f = liftIO $ atomically $ do
    state <- readTVar (getIncredibleStore store)
    case Map.lookupMax (machines state) of
      Nothing -> error "initial machine missing from map"
      Just (version, current) -> do
        let (r, nv) = f $ VersionedMachine version current
        let newVer = succ version
        when (current /= nv) $
          writeTVar (getIncredibleStore store) $ state { machines = Map.insert newVer nv $ machines state }
        pure r

  addBlueprint' store blueprint = liftIO $ atomically $ do
    state <- readTVar (getIncredibleStore store)
    writeTVar (getIncredibleStore store) $ state { blueprints = Map.insert (blueprintID blueprint) blueprint $ blueprints state }
  getBlueprint' store bpID = liftIO $ do
    state <- readTVarIO $ getIncredibleStore store
    pure $ Map.lookup bpID $ blueprints state

  addSnapshot' store blueprint snapshot = liftIO $ atomically $ do
    state <- readTVar (getIncredibleStore store)
    writeTVar (getIncredibleStore store) $ state { snapshots = Map.insert blueprint snapshot $ snapshots state }
  getSnapshot' store bpID = liftIO $ do
    state <- readTVarIO $ getIncredibleStore store
    pure $ Map.lookup bpID $ snapshots state

  queueModeration' store pzlID bpID = liftIO $ atomically $ do
    state <- readTVar (getIncredibleStore store)
    let insertModeration = Map.insertWith Set.union pzlID (Set.singleton bpID)
    writeTVar (getIncredibleStore store) $ state { moderation = insertModeration $ moderation state }
  dequeueModeration' store pzlID bpID = liftIO $ atomically $ do
    state <- readTVar (getIncredibleStore store)
    let removeModeration = Map.adjust (Set.delete bpID) pzlID
    writeTVar (getIncredibleStore store) $ state { moderation = removeModeration $ moderation state }
  listModerationQueue' store pzlID = liftIO $ do
    state <- readTVarIO $ getIncredibleStore store
    pure $ maybe [] Set.toList $ Map.lookup pzlID $ moderation state

  addWorkOrder' store wid w = liftIO $ atomically $ do
    state <- readTVar (getIncredibleStore store)
    writeTVar (getIncredibleStore store) $ state { workorders = Map.insert wid w $ workorders state }
  pullWorkOrder' store wid = liftIO $ atomically $ do
    state <- readTVar $ getIncredibleStore store
    let (mwo, nm) = Map.updateLookupWithKey (\_ _ -> Nothing) wid $ workorders state
    writeTVar (getIncredibleStore store) $ state { workorders = nm }
    pure mwo

  widgetOrderBook' store ws = liftIO $ atomically $ do
    state <- readTVar (getIncredibleStore store)
    let nm = Map.insertWith (+) ws 1 $ widgetOrders state
    writeTVar (getIncredibleStore store) $ state { widgetOrders = nm }
    pure $ Map.findWithDefault 1 ws nm
