{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Incredible.Config
import qualified Incredible.DataStore.Memory as MemoryStore
import qualified Incredible.DataStore.Redis as IRedis
import           Incredible.App
import           Network.Wai.Handler.Warp
import           Options.Applicative

data IncredibleOptions = IncredibleOptions {
  incredibleConfigPath :: FilePath
, incredibleMachinePath :: FilePath
, incredibleDBMem :: Bool
}

incredibleOpts :: Parser IncredibleOptions
incredibleOpts = do
  IncredibleOptions <$> configPath <*> machinePath <*> useMem
  where
    configPath = strOption $ mconcat [
                      long "config"
                    , short 'c'
                    , metavar "CONFIG"
                    , value "config/incredible.toml"
                    , help "Path to the incredible config file"
                    ]
    machinePath = strOption $ mconcat [
                      long "machine"
                    , short 'm'
                    , metavar "MACHINE"
                    , value "config/machine.json"
                    , help "Path to the incredible machine file"
                    ]
    useMem = flag False True $ mconcat [
                      long "mem"
                    , help "if we should use an in-memory datastore."
                    ]

main :: IO ()
main = do
  putStrLn "Starting Incredible..."
  opts <- execParser $ info (incredibleOpts <**> helper) fullDesc
  putStrLn $ "Using config file: " ++ incredibleConfigPath opts
  cnf <- readIncredibleConfig $ incredibleConfigPath opts
  putStrLn $ "Using machine file: " ++ incredibleMachinePath opts
  puzzleMachine <- loadPuzzleMachine $ incredibleMachinePath opts
  putStrLn $ "Init incredible app..."
  webApp <- if incredibleDBMem opts
             then incredibleApp <$> openMachineShop cnf puzzleMachine MemoryStore.initIncredibleState
             else incredibleApp <$> openMachineShop cnf puzzleMachine IRedis.initIncredibleState
  let runWeb = run (incredibleWebPort $ incredibleWebConfig cnf) webApp
  putStrLn $ "Running on port: " ++ show (incredibleWebPort $ incredibleWebConfig cnf)
  runWeb
