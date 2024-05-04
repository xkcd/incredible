module Main where

import           Control.Monad
import           Incredible.App (writePuzzleMachine)
import           Incredible.Data
import           Incredible.Puzzle
import           Options.Applicative
import           System.Random
import System.Random.Stateful


data GenConfig = GenConfig {
  genSeed :: Maybe Int
, genTest :: Bool
} deriving (Eq, Ord, Show)

genOpts :: Parser GenConfig
genOpts = do
  GenConfig <$> configSeed <*> configTest
  where
    configSeed = option auto $ mconcat [
                      long "seed"
                    , short 's'
                    , metavar "SEED"
                    , value Nothing
                    , help "Seed for the random number generator"
                    ]
    configTest = flag False True $ mconcat [
                      long "test"
                    , short 't'
                    , help "Generate a smaller test puzzle"
                    ]

main :: IO ()
main = do
  opts <- execParser $ info (genOpts <**> helper) fullDesc
  seed <- case genSeed opts of
    Just s -> pure s
    Nothing -> randomIO
  putStrLn $ "Using seed: " <> show seed
  let
    (label, puzzle) = if genTest opts then ("test-machine-", testPuzzle) else ("machine-", gamePuzzle)
    fp = label <> show seed <> ".json"
  gen <- newIOGenM $ mkStdGen seed
  generated <- generatePuzzle gen puzzle
  let m = mm generated
  possible <- machineIsSolvable m
  when (not possible) $ fail "Machine is not solvable"
  writePuzzleMachine fp m
  putStrLn "Machine written"
  where
    mm arr = MetaMachine arr (TileSize 740 740) 1000 mempty