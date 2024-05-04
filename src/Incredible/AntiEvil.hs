{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Certainly not good.
module Incredible.AntiEvil
( issueWorkOrder
, checkWorkOrder
) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.Text as T
import           Incredible.Data
import           Incredible.DataStore

issueWorkOrder :: (MonadIO m, IncredibleData s, MonadReader (MachineShop s) m) => [PuzzleID] -> m T.Text
issueWorkOrder _ = pure "test-work-order"

checkWorkOrder :: (MonadIO m, IncredibleData s, MonadReader (MachineShop s) m) => Maybe T.Text -> Blueprint -> m Bool
checkWorkOrder _ _ = pure True
