{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Incredible.Puzzle where

import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Control.Monad.ST
import qualified Data.Aeson.KeyMap as JS
import           Data.Array (Array)
import qualified Data.Array as Array
import           Data.Array.IO (IOArray)
import qualified Data.Array.MArray as MArray
import           Data.Bifunctor
import           Data.Foldable
import qualified Data.Foldable as Foldable
import           Data.IORef
import qualified Data.Ix as Ix
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, fromJust)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MVector
import           GHC.IsList
import           Incredible.Data
import qualified Incredible.DataStore.Memory as Mem
import           System.Random.Stateful
import           Text.Printf                 (printf)

data PuzzleConfig = PuzzleConfig {
  ballTypes            :: Int -- ^ Number of different types of balls
, minBallConfiguration :: [Int] -- ^ Minimum ball configuration
, puzzleLocations      :: Set Double -- ^ Locations of possible connectors
}

data PuzzleGen = PuzzleGen {
  puzzleConfig   :: PuzzleConfig
, puzzleGenerate :: IOGenM StdGen -> IO [PuzzleRow]
}

gamePuzzle :: PuzzleGen
gamePuzzle = PuzzleGen cfg $ \g -> do
  applyAll initial $ concat [
        replicate 10 $ swapBalls g (SwapConfig (1,2) (14,20)) -- >=> towardsBallCount g cfg 16
      , replicate 10 $ swapBalls g (SwapConfig (1,4) (18,20))
      , replicate 10 $ swapBalls g (SwapConfig (1,4) (18,18)) -- >=> towardsBallCount g cfg 18
      , replicate 10 $ swapBalls g (SwapConfig (1,4) (12,12))
      , replicate 10 $ swapBalls g (SwapConfig (1,4) (10,10)) -- >=> towardsBallCount g cfg 16
      , replicate 10 $ swapBalls g (SwapConfig (1,4) (16,16))
      , replicate 10 $ swapBalls g (SwapConfig (1,4) (16,16)) -- >=> towardsBallCount g cfg 14
      , replicate 10 $ swapBalls g (SwapConfig (1,4) (16,16))
      , replicate 10 $ swapBalls g (SwapConfig (1,4) (10,10)) -- >=> towardsBallCount g cfg 10
      , replicate 10 $ swapBalls g (SwapConfig (1,4) (5,5)) -- >=> towardsBallCount g cfg 10
      , replicate 10 $ swapBalls g (SwapConfig (1,4) (5,5)) -- >=> towardsBallCount g cfg 10
      , replicate 10 $ swapBalls g (SwapConfig (1,4) (5,5)) -- >=> towardsBallCount g cfg 10
      , replicate 7 $ swapBalls g (SwapConfig (1,4) (5,5)) -- >=> towardsBallCount g cfg 10
      -- , replicate 1 $ pure
    ]
  where
    cfg = PuzzleConfig 4 ballConfiguration (Set.fromList [0.2, 0.35, 0.5, 0.65, 0.8])
    initial = PuzzleRow $ Vector.fromList $ concat $ fmap mkColumm ballConfiguration
    ballConfiguration = [2,2,2, 4,4,4, 1,1,1, 3,3,3]
    -- mkColumm b = [ConnectorEmpty, ConnectorOpen $ BallType' b,  ConnectorEmpty, ConnectorOpen $ BallType' b, ConnectorEmpty]
    mkColumm b = [ConnectorEmpty, ConnectorEmpty, ConnectorOpen $ BallType' b,  ConnectorEmpty, ConnectorEmpty]

testPuzzle :: PuzzleGen
testPuzzle = PuzzleGen cfg $ \g -> do
  applyAll initial $ concat [
        replicate 3 $ swapBalls g (SwapConfig (1,2) (10,15)) >=> towardsBallCount g cfg 5
      , replicate 3 $ swapBalls g (SwapConfig (1,2) (10,15)) >=> towardsBallCount g cfg 6
      , replicate 3 $ swapBalls g (SwapConfig (1,2) (10,15)) >=> towardsBallCount g cfg 5
      , replicate 1 $ pure
    ]
  where
    cfg = PuzzleConfig 4 ballConfiguration (Set.fromList [0.2, 0.35, 0.5, 0.65, 0.8])
    ballConfiguration = [1,2,3,3,4]
    initial = PuzzleRow $ Vector.fromList $ concat $ fmap mkColumm [1,2,3,3,4]
    mkColumm b = [ConnectorEmpty, ConnectorOpen $ BallType' b,  ConnectorEmpty, ConnectorOpen $ BallType' b, ConnectorEmpty]

generatePuzzle :: IOGenM StdGen -> PuzzleGen -> IO (Array (X, Y) Puzzle)
generatePuzzle g gen = do
  retryForever $ do
    rows <- puzzleGenerate gen g
    putStrLn "Rendingering rows to grid"
    outputs <- fromOutputRows cfg rows
    putStrLn "Routing balls"
    routed <- routeBalls g outputs
    putStrLn "Placing connecteors"
    placed <- routeConnectors g (puzzleLocations cfg) routed

    putStrLn "Joinging streams"
    joined <- joinStreams g 30 placed

    putStrLn "Calculating rates"
    rates <- calculateRates joined

    putStrLn "Calculating dependencies"
    dependencies <- generateDependencies placed

    let puzzle = iMap (toPuzzle' dependencies rates) joined
    when (any isEmptyPuzzle puzzle) $ fail "Empty puzzle"
    putStrLn (renderRows rows)
    putStrLn $ prettyDependencyArray' dependencies
    putStrLn "--------------------------------"
    putStrLn $ prettyPuzzleRates (bimap (round . (*10)) (round . (*10))) puzzle
    putStrLn "--------------------------------"
    putStrLn $ prettyPuzzleArray' (bimap (round . (*10)) (round . (*10))) $ puzzle
    pure puzzle
  where
    retryForever f =
      try f >>= \case
        Left (e :: SomeException) -> do
          putStrLn $ "Error: " <> show e
          retryForever f
        Right r -> pure r
    cfg = puzzleConfig gen
    iMap f r = Array.array (Array.bounds r) $ fmap (\(i, a) -> (i, f i a)) $ Array.assocs r

isEmptyPuzzle :: Puzzle -> Bool
isEmptyPuzzle p = Vector.null (pInputs p) && Vector.null (pOutputs p)

toPuzzle'' :: (X,Y) -- ^ Location of the puzzle, if the puzzle is on the top row, it has no requirements before it can be solved
          -> PuzzleConnectors (Set Double) -- ^ Connectors for the puzzle, each connector direction has the ratio from the origin of the edge and the type of the connector
          -> Puzzle
toPuzzle'' (_x, y) (PuzzleConnectors i o) =
  Puzzle
    (Vector.fromList requirements)
    (mkGateways inputs)
    (mkGateways outputs)
    JS.empty -- TODO add spec
  where
    requirements =
      if y == 0
        then [] -- no requirements for the top row
        else (fmap dirToRel $ Map.keys $ Map.filter Set.null $ inputs) -- TODO inputs are required before the puzzle can be solved
    mkGateways :: Map Direction (Set Double) -> Vector.Vector Gateway
    mkGateways = Vector.fromList . concat . fmap (\(dir, rs) -> fmap (toGateway dir) $ Set.toList rs) . Map.toList
    toGateway :: Direction -> Double -> Gateway
    toGateway dir r  = Gateway (onEdge dir r) (Vector.fromList [GatewayBall 1 1]) -- TODO add rates
    inputs = directed i
    outputs = directed o

machineIsSolvable :: MetaMachine Puzzle -> IO Bool
machineIsSolvable gen = do
  (_, ps) <- Mem.initIncredibleState () gen
  pure $ isSolvable ps gs
  where
    gs = fmap (Left . puzzleID) gen

ratesCount :: IOArray (X,Y) (PuzzleConnectors (Map Double Double)) -> IO Int
ratesCount rates = do
  bounds <- MArray.getBounds rates
  fmap sum $ forM (Ix.range bounds) $ \i -> do
    p <- MArray.readArray rates i
    let is = puzzleInputs p
        isCount :: Int
        isCount = sum $ Map.elems $ fmap Map.size $ directed is
        os = puzzleOutputs p
        osCount = sum $ Map.elems $ fmap Map.size $ directed os
    pure $ osCount + isCount

calculateRates :: Array (X,Y) (PuzzleConnectors (Map Double (Set Int)))
               -> IO (Array (X,Y) (PuzzleConnectors (Map Double  Double)))
calculateRates grid' = do
  rates :: IOArray (X, Y) (PuzzleConnectors (Map Double Double)) <- MArray.thaw $ fmap (fmap (fmap (const 0.0))) grid'

  let
    propagateFrom :: IORef (Set (X,Y)) -> Int -> (X, Y) -> IO ()
    propagateFrom visited ty loc = do
      rs <- MArray.readArray rates loc
      vs <- readIORef visited
      if loc `Set.member` vs
      then pure ()
      else do
        atomicModifyIORef' visited $ \v -> (Set.insert loc v, ())

        let
          connectors = grid' Array.! loc
          inputConnectors = fmap (Map.filter (Set.member ty)) $ puzzleInputs connectors
          inputLocations :: [(Direction, Double)]
          inputLocations = concat $ fmap (\(dir, m) -> (dir,) <$> Map.keys m) $ Map.toList $ directed inputConnectors
          outputConnectors = fmap (Map.filter (Set.member ty)) $ puzzleOutputs connectors
          outputCount = sum $ fmap Set.size $ concat $ fmap Map.elems $ Map.elems $ directed outputConnectors
          inputRate = sum $ fmap (\(dir, edgeLocation) -> Map.findWithDefault 0 edgeLocation  $ inDirection dir $ puzzleInputs rs ) inputLocations
          !outputRate = inputRate / fromIntegral outputCount
        -- print (loc, "inputRate", inputRate, "outputRate", outputRate, "outputCount", outputCount)
        res <- fmap (Set.fromList . catMaybes) $ forM (Map.toList $ directed outputConnectors) $ \(dir, edges) -> do
          getRelativeM rates loc dir >>= \case
              Nothing -> do
                forM_ (Map.toList edges) $ \(edgeLocation, _) -> do
                  MArray.modifyArray' rates loc $ modifyOutputs (modifyDirection dir (Map.insert edgeLocation outputRate))
                pure Nothing
              Just (nLoc, _) -> do
                forM_ (Map.toList edges) $ \(edgeLocation, _) -> do
                  -- print ("propagating", loc, nLoc, outputRate, dir)
                  MArray.modifyArray' rates loc $ modifyOutputs (modifyDirection dir (Map.insert edgeLocation outputRate))
                  MArray.modifyArray' rates nLoc $ modifyInputs (modifyDirection (switchDirection dir) (Map.insert edgeLocation outputRate))
                pure $ Just nLoc
        forM_ res $ propagateFrom visited ty
      -- propagateFrom (Set.insert nLoc visited) ty nLoc

  -- Set all of the top inputs of a rate of 1.0
  forM_ topInputs $ \(loc, is) -> do
    forM_ is $ \(edgeLocation, _ts) -> do
      MArray.modifyArray' rates loc $ modifyInputs (modifyDirection DUp (Map.insert edgeLocation 1.0))

  let
    iterateUntilStable :: Int -> IO () -> IO ()
    iterateUntilStable 0 _ = error "Failed to converge"
    iterateUntilStable n act = do
      before :: Array (X, Y) (PuzzleConnectors (Map Double Double)) <- MArray.freeze rates
      act
      after <- MArray.freeze rates
      if before == after
        then pure ()
        else iterateUntilStable (n - 1) act

  iterateUntilStable 10000 $ forM_ topInputs $ \(loc, is) -> do
    forM_ is $ \(_edgeLocation, ts) -> do
      forM_ ts $ \t -> do
        visited <- newIORef Set.empty
        propagateFrom visited t loc

  MArray.freeze rates
  where
    ((startX, startY), (endX, _endY)) = Array.bounds grid'
    topInputs = flip fmap [startX .. endX ] $ \x ->
      let loc = (x, startY)
      in (loc, fmap (\(k, v) -> (k, Set.toList v)) $ Map.toList $ inDirection DUp $ puzzleInputs $ grid' Array.! loc)

-- calculateRates' :: Array (X,Y) (PuzzleConnectors (Map Double (Set Int)))
--                 -> IO (Array (X,Y) (PuzzleConnectors (Map Double  Double)))
-- calculateRates' grid' = do
--   rates :: IOArray (X, Y) (PuzzleConnectors (Map Double Double)) <- MArray.thaw $ fmap (fmap (fmap (const 0.0))) grid'

--   let
--     propagateRates :: (X, Y) -> IO ()
--     propagateRates loc = do
--       rs <- MArray.readArray rates loc

--       forM_ (Set.toList tys) $ \ty -> do
          
--         let
--           connectors = grid' Array.! loc
--           inputConnectors = fmap (Map.filter (Set.member ty)) $ puzzleInputs connectors
--           inputLocations :: [(Direction, Double)]
--           inputLocations = concat $ fmap (\(dir, m) -> (dir,) <$> Map.keys m) $ Map.toList $ directed inputConnectors
--           outputConnectors = fmap (Map.filter (Set.member ty)) $ puzzleOutputs connectors
--           outputCount = sum $ fmap Set.size $ concat $ fmap Map.elems $ Map.elems $ directed $ outputConnectors
--           inputRate = sum $ fmap (\(dir, edgeLocation) -> Map.findWithDefault 0 edgeLocation  $ inDirection dir $ puzzleInputs rs ) $ inputLocations
--           outputRate = inputRate / fromIntegral outputCount
--         forM_ (Map.toList $ directed outputConnectors) $ \(dir, cs) -> 
--           forM_ (Map.toList cs) $ \(edgeLocation, _) -> do
--             getRelativeM rates loc dir >>= \case
--               Nothing -> do
--                 MArray.modifyArray' rates loc $ modifyOutputs (modifyDirection dir (Map.insert edgeLocation outputRate))
--               Just (nLoc, _) -> do
--                   MArray.modifyArray' rates loc $ modifyOutputs (modifyDirection dir (Map.insert edgeLocation outputRate))
--                   MArray.modifyArray' rates nLoc $ modifyInputs (modifyDirection (switchDirection dir) (Map.insert edgeLocation outputRate))
--         pure ()


  -- Set all of the top inputs of a rate of 1.0
  -- forM_ topInputs $ \(loc, is) -> do
  --   forM_ is $ \(edgeLocation, _ts) -> do
  --     MArray.modifyArray' rates loc $ modifyInputs (modifyDirection DUp (Map.insert edgeLocation 1.0))

  -- let
  --   iterateUntilStable :: Int -> IO () -> IO ()
  --   iterateUntilStable 0 _ = pure ()
  --   iterateUntilStable n act = do
  --     act
  --     iterateUntilStable (n - 1) act

  -- iterateUntilStable 50 $
  --   forM_ [startY .. endY] $ \y ->
  --     forM_ [startX, endX] $ \x -> do
  --       let loc = (x, y)
  --       -- putStrLn $ "Propagating rates from " <> show loc
  --       propagateRates loc

  -- MArray.freeze rates
  -- where
  --   tys :: Set Int
  --   tys = Set.unions $ fmap (Set.unions . concat . fmap Map.elems . Map.elems . directed . puzzleInputs) $ Array.elems grid'
  --   ((startX, startY), (endX, endY)) = Array.bounds grid'
  --   topInputs = flip fmap [startX .. endX ] $ \x ->
  --     let loc = (x, startY)
  --     in (loc, fmap (\(k, v) -> (k, Set.toList v)) $ Map.toList $ inDirection DUp $ puzzleInputs $ grid' Array.! loc)

joinStreams :: IOGenM StdGen -> Int -> Array (X, Y) (PuzzleConnectors (Map Double Int)) -> IO (Array (X, Y) (PuzzleConnectors (Map Double (Set Int))))
joinStreams g den puzzle' = do
  puzzle :: (IOArray (X, Y) (PuzzleConnectors (Map Double (Set Int)))) <- MArray.thaw $ fmap (fmap Set.singleton) <$> puzzle'

  forM_ (Ix.range $ Array.bounds puzzle') $ \i -> do
    res <- randomRM (1, den) g
    p <- MArray.readArray puzzle  i
    if res == 1
      then do
        let validJoins = Map.toList $ Map.filter (\cs -> Map.size cs > 1) $ directed $ puzzleOutputs p
        picked <- pickOne g validJoins
        case picked of
          Nothing -> pure ()
          Just (dir, pick1) -> do
            pick2 <- pickOne' g pick1
            case pick2 of
              Nothing -> pure ()
              Just (rest1, (chosenLoc, cs)) -> do
                pick3 <- pickOne' g rest1
                case pick3 of
                  Nothing -> pure ()
                  Just (rest2, (_chosenLoc2, cs2)) -> do
                    getRelativeM puzzle i dir >>= \case
                      Nothing -> pure ()
                      Just (nLoc, neighorP) -> do
                        -- putStrLn $ "Joining " <> show i <> " " <> show nLoc <> " " <> show dir
                        let
                          new' :: Map Double (Set Int)
                          new' = Map.insertWith Set.union chosenLoc (Set.union cs cs2) rest2
                        MArray.writeArray puzzle i $ modifyOutputs (modifyDirection dir (const new')) p
                        MArray.writeArray puzzle nLoc $ modifyInputs (modifyDirection (switchDirection dir) (const new')) neighorP
      else pure ()
  MArray.freeze puzzle


-- bubbleSortStep :: PuzzleRow -> IO PuzzleRow
-- bubbleSortStep row = do
--   let
--     balls = rowBalls row
--     swaps = Vector.ifoldl' (\acc i (i', b) -> if i /= i' then (i, i') : acc else acc) [] $ Vector.indexed balls
--   pure $ modifyPuzzleRow row $ \v -> do
--     forM_ swaps $ \(i, j) -> do
--       let
--         (iB, _) = balls Vector.! i
--         (jB, _) = balls Vector.! j
--       MVector.swap v iB jB



prettyDependencyArray' :: Array (X, Y) (Set Direction) -> String
prettyDependencyArray' a =
  unlines $ concat $ flip fmap [startY..maxY] $ \y -> concatRows $ flip fmap [startX..maxX] $ \x -> prettyDependency $ a Array.! (x, y)
  where
    ((startX, startY), (maxX, maxY)) = Array.bounds a
    concatRows :: [[String]] -> [String]
    concatRows = fmap concat . List.transpose

prettyDependency :: Set Direction -> [String]
prettyDependency dirs = top ++ res ++ bottom
  where
    top = [replicate (length $ head res) '-']
    bottom = [replicate (length $ head res) '-']
    res = [
        ['|', '.', depAt DUp, '.','|']
      , ['|', depAt DLeft, '.', depAt DRight, '|']
      , ['|', '.', depAt DDown, '.', '|']
      ]
    depAt dir = if dir `Set.member` dirs then 'X' else '.'

-- | Gets the value in a cell at a relative position to a given cell, or Nothing if that is outside the grid.
getRelativeM :: IOArray (X,Y) a -> (X, Y) -> Direction -> IO (Maybe ((X,Y), a))
getRelativeM g (x0, y0) r = do
  bounds <- MArray.getBounds g
  if Ix.inRange bounds rp
    then do
      (Just . (rp,)) <$> MArray.readArray g rp
    else pure Nothing
  where
    rp = bimap (x0 +) (y0 +) $ relMap r

generateDependencies :: Array (X, Y) (PuzzleConnectors (Map Double Int)) -> IO (Array (X,Y) (Set Direction))
generateDependencies routed = do
  -- Track the dependencies for each puzzle
  depencdencies :: IOArray (X, Y) (Set Direction) <- MArray.newGenArray bounds $ const $ pure mempty

  let
    go loc = do
      let
        outputs = directed $ puzzleOutputs $ routed Array.! loc
        outputs' = Map.filter (not . Map.null) outputs
      flip mapM_ (Map.keys outputs') $ \dir -> do
        localDeps <- MArray.readArray depencdencies loc
        if dir `Set.member` localDeps
          then pure ()
          else do
            getRelativeM depencdencies loc dir >>= \case
              Nothing -> pure ()
              Just (nLoc, neighborDependencies)
                | (switchDirection dir) `Set.member` neighborDependencies -> pure ()
                | otherwise -> do
                  MArray.writeArray depencdencies nLoc $ Set.insert (switchDirection dir) neighborDependencies
                  go nLoc

  flip mapM_ [xStart .. xEnd] $ \x -> do
    go (x, yStart)

  MArray.freeze depencdencies
  where
    bounds@((xStart, yStart), (xEnd, _yEnd)) = Array.bounds routed


prettyPuzzleRates :: ((Double, Double) -> (Int, Int)) -> Array (X, Y) Puzzle -> String
prettyPuzzleRates connIndex a =
  unlines $ concat $ flip fmap [startY..maxY] $ \y -> concatRows $ flip fmap [startX..maxX] $ \x -> prettyPuzzleRates' connIndex $ a Array.! (x, y)
  where
    ((startX, startY), (maxX, maxY)) = Array.bounds a
    concatRows :: [[String]] -> [String]
    concatRows = fmap concat . List.transpose

prettyPuzzleArray' :: ((Double, Double) -> (Int, Int)) -> Array (X, Y) Puzzle -> String
prettyPuzzleArray' connIndex a =
  unlines $ concat $ flip fmap [startY..maxY] $ \y -> concatRows $ flip fmap [startX..maxX] $ \x -> prettyPuzzle' connIndex $ a Array.! (x, y)
  where
    ((startX, startY), (maxX, maxY)) = Array.bounds a
    concatRows :: [[String]] -> [String]
    concatRows = fmap concat . List.transpose

prettyPuzzle' :: (Position -> (Int, Int)) -> Puzzle -> [String]
prettyPuzzle' connIndex p = top ++ res ++ bottom
  where
    top = [replicate (length $ head res) '-']
    bottom = [replicate (length $ head res) '-']
    res =
      fmap (("|" ++) . (++ "|")) $
        flip fmap [0..height ] $ \y ->
          concat $ flip fmap [0..width] $ \xp ->
            case Map.lookup (xp, y) is of
              Nothing ->
                case Map.lookup (xp, y) os of
                  Nothing -> "..."
                  Just o  -> show $ Vector.toList $ gBalls o
              Just _i -> "III" -- show $ UVector.head $ gType i
            -- if (xp, y) `Set.member` is then "I" else if (xp, y) `Set.member` os then "O" else "."
    os = Map.fromList $ Vector.toList $ fmap ((connIndex .  gPos) &&& id) $ pOutputs p
    is = Map.fromList $ Vector.toList $ fmap ((connIndex . gPos) &&& id) $ pInputs p
    (width, height) = connIndex (1, 1)

prettyPuzzleRates' :: (Position -> (Int, Int)) -> Puzzle -> [String]
prettyPuzzleRates' connIndex p = top ++ res ++ bottom
  where
    top = [replicate (length $ head res) '-']
    bottom = [replicate (length $ head res) '-']
    res =
      fmap (("|" ++) . (++ "|")) $
        flip fmap [0..height ] $ \y ->
          concat $ flip fmap [0..width] $ \xp ->
            case Map.lookup (xp, y) is of
              Nothing ->
                case Map.lookup (xp, y) os of
                  Nothing -> "...."
                  Just o  -> showDec o
              Just i -> showDec i -- show $ UVector.head $ gType i
            -- if (xp, y) `Set.member` is then "I" else if (xp, y) `Set.member` os then "O" else "."
    os = Map.fromList $ Vector.toList $ fmap ((connIndex .  gPos) &&& id) $ pOutputs p
    is = Map.fromList $ Vector.toList $ fmap ((connIndex . gPos) &&& id) $ pInputs p
    (width, height) = connIndex (1, 1)
    showDec = printf "%.2f" . sum . fmap gbRate . Vector.toList . gBalls

newtype BallType' = BallType' {
  unBallType :: Int
} deriving (Eq, Ord, Show)

-- | Create a puzzle from connectors
toPuzzle :: Array (X, Y) (Set Direction) -- ^ Dependencies for each puzzle
         -> (X,Y) -- ^ Location of the puzzle, if the puzzle is on the top row, it has no requirements before it can be solved
         -> PuzzleConnectors (Map Double Int) -- ^ Connectors for the puzzle, each connector direction has the ratio from the origin of the edge and the type of the connector
         -> Puzzle
toPuzzle dependencies loc@(_x, y) (PuzzleConnectors i o) =
  Puzzle
    (Vector.fromList requirements)
    (mkGateways inputs)
    (mkGateways outputs)
    JS.empty -- TODO add spec
  where
    requirements =
      if y == 0
        then [] -- no requirements for the top row
        else fmap dirToRel $ Set.toList $ dependencies Array.! loc
    mkGateways :: Map Direction (Map Double Int) -> Vector.Vector Gateway
    mkGateways = Vector.fromList . concat . fmap (\(dir, rs) -> fmap (uncurry $ toGateway dir) $ Map.toList rs) . Map.toList
    toGateway :: Direction -> Double -> Int -> Gateway
    toGateway dir r t = Gateway (onEdge dir r) (Vector.fromList [GatewayBall t 1]) -- TODO add rates
    inputs = directed i
    outputs = directed o

-- | Create a puzzle from connectors
toPuzzle' :: Array (X, Y) (Set Direction) -- ^ Dependencies for each puzzle
          -> Array (X, Y) (PuzzleConnectors (Map Double Double))
         -> (X,Y) -- ^ Location of the puzzle, if the puzzle is on the top row, it has no requirements before it can be solved
         -> PuzzleConnectors (Map Double (Set Int)) -- ^ Connectors for the puzzle, each connector direction has the ratio from the origin of the edge and the type of the connector
         -> Puzzle
toPuzzle' dependencies allRates loc@(_x, y) (PuzzleConnectors i o) =
  Puzzle
    (Vector.fromList requirements)
    (mkGateways inputRates inputs)
    (mkGateways outputRates outputs)
    JS.empty -- TODO add spec
  where
    requirements =
      if y == 0
        then [] -- no requirements for the top row
        else fmap dirToRel $ Set.toList $ dependencies Array.! loc
    mkGateways :: DirectedConnectors (Map Double Double) -> Map Direction (Map Double (Set Int)) -> Vector.Vector Gateway
    mkGateways rates = Vector.fromList . concat . fmap (\(dir, rs) -> fmap (uncurry $ toGateway rates dir) $ Map.toList rs) . Map.toList
    toGateway :: DirectedConnectors (Map Double Double) -> Direction -> Double -> Set Int -> Gateway
    toGateway rates dir r t = Gateway (onEdge dir r) (Vector.fromList $ fmap (`GatewayBall` (lookupRate rates dir r)) $ Set.toList t) -- TODO add rates
    lookupRate rates d r = fromJust $ Map.lookup r $ inDirection d rates
    inputRates = puzzleInputs $ allRates Array.! loc
    outputRates = puzzleOutputs $ allRates Array.! loc
    inputs = directed i
    outputs = directed o

randomBallType :: IOGenM StdGen -> PuzzleConfig -> IO BallType'
randomBallType gen cfg = do
  i <- randomRM (1, ballTypes cfg) gen
  pure $ BallType' i

data PuzzleConnector =
    ConnectorEmpty
  | ConnectorOpen BallType'
  deriving (Eq, Ord, Show)

toBallType :: PuzzleConnector -> Maybe BallType'
toBallType ConnectorEmpty    = Nothing
toBallType (ConnectorOpen i) = Just i

newtype PuzzleRow = PuzzleRow {
  unPuzzleRow :: Vector PuzzleConnector -- ^ The row of outputs for a puzzle
} deriving (Eq, Ord, Show)

newtype OutputColumn = OutputColumn {
  unOutputColumn :: PuzzleConnectors (Set BallType')
} deriving (Eq, Ord, Show)

pathsTo :: IOGenM StdGen -> [(X, BallType')] -> [(X, BallType')] -> IO (Map BallType' [(X, Int)])
pathsTo g from' to' = do
  fmap (Map.fromListWith (<>)) $ flip mapM tys $ \ty -> do
    let xByType l = fmap fst $ List.filter ((== ty) . snd) l
    (from, to) <- bimap List.sort List.sort <$> matchLists g ("\n" <> show tys <> "\n" <> show from' <> "\n" <> show to') 0 (xByType from') (xByType to')
    pure $ (ty, zipWith go from to)
  where
    go x1 x2 = (x1, x2 - x1)
    tys = Set.toList $ Set.fromList $ (snd <$> from') <> (snd <$> to')

matchLists :: Show a => IOGenM StdGen -> String -> Int -> [a] -> [a] -> IO ([a], [a])
matchLists g deb n xs ys
  | length xs == 0 || length ys == 0 = error $ "Empty lists " <> deb <> show n <> " - " <> show (xs, ys)
  | length xs == length ys = pure (xs, ys)
  | length xs > length ys = do
    ys' <- (:ys) . (ys !!) <$> randomRM (0, length ys - 1) g
    matchLists g deb (n + 1) xs ys'
  | length xs < length ys = do
    xs' <- (:xs) . (xs !!) <$> randomRM (0, length xs - 1) g
    matchLists g deb (n + 1) xs' ys

  | otherwise = pure (xs, ys)

routeConnectors :: IOGenM StdGen
                -> Set Double
                -> Array (X, Y) (PuzzleConnectors (Vector Int))
                -> IO (Array (X, Y) (PuzzleConnectors (Map Double Int)))
routeConnectors g locations placed' = do
  placed <- MArray.thaw placed'

  routed :: IOArray (X, Y) (PuzzleConnectors (Map Double Int)) <- MArray.newGenArray bounds $ const $ pure emptyConnectors
  -- TODO add crossing complexities
  forM_ (reverse [0..yMax]) $ \y -> do
    forM_ ([0..xMax]) $ \x -> do
      let loc = (x,y)
      p <- MArray.readArray placed loc
      let inputs = directed $ puzzleInputs p
          outputs = directed $ puzzleOutputs p
      MArray.modifyArray placed loc $ modifyInputs (const $ DirectedConnectors mempty mempty mempty mempty) . modifyOutputs (const $ DirectedConnectors mempty mempty mempty mempty)
      forM_ directions $ \dir -> do
        (leftoverLocations, is) <- pickN' g locations $ Vector.toList $ Map.findWithDefault Vector.empty dir inputs
        MArray.modifyArray routed loc $ modifyInputs (modifyDirection dir ((Map.union $ Map.fromList is)))
        (_, os) <- pickN' g leftoverLocations $ Vector.toList $ Map.findWithDefault Vector.empty dir outputs
        MArray.modifyArray routed loc $ modifyOutputs (modifyDirection dir ((Map.union $ Map.fromList os)))
        getRelativeM placed loc dir >>= \case
          Nothing -> pure () -- no neighbor
          Just (nLoc, _) -> do
            MArray.modifyArray routed nLoc $ modifyInputs (modifyDirection (switchDirection dir) ((Map.union $ Map.fromList os)))
            MArray.modifyArray placed nLoc $ modifyInputs (modifyDirection (switchDirection dir) $ const mempty)
            MArray.modifyArray routed nLoc $ modifyOutputs (modifyDirection (switchDirection dir) ((Map.union $ Map.fromList is)))
            MArray.modifyArray placed nLoc $ modifyOutputs (modifyDirection (switchDirection dir) $ const mempty)
        pure (is, os)
  MArray.freeze routed
  where
    bounds@(_, (xMax, yMax)) = Array.bounds placed'
    emptyConnectors = PuzzleConnectors emptyDirected emptyDirected
    emptyDirected = DirectedConnectors mempty mempty mempty mempty

-- | Pick n elements from a set
pickN' :: (Ord a, Show a, Show i) => IOGenM StdGen -> Set a -> [i] -> IO (Set a, [(a, i)])
pickN' _ s [] = pure (s, mempty)
pickN' g s (x:xs)
  | Set.null s = pure (mempty, [])
  | otherwise = do
    i <- randomRM (0, Set.size s - 1) g
    let selected = Set.elemAt i s
    (s', rest) <- pickN' g (Set.deleteAt i s) xs
    pure $ (s', (selected,x):rest)

-- | Pick n elements from a set
pickN :: Ord a => IOGenM StdGen -> Set a -> Int-> IO (Set a)
pickN g s n
  | n <= 0 = pure mempty
  | Set.null s = pure mempty
  | otherwise = do
    i <- randomRM (0, Set.size s - 1) g
    let selected = Set.elemAt i s
    rest <- pickN g (Set.deleteAt i s) (n - 1)
    pure $ Set.insert selected rest

-- | Pick n elements from a set
pickOne :: IOGenM StdGen -> [i] -> IO (Maybe i)
pickOne g xs
  | List.null xs = pure Nothing
  | otherwise = do
    i <- randomRM (0, length xs - 1) g
    pure $ Just $ xs List.!! i

pickOne' :: IOGenM StdGen -> Map a b -> IO (Maybe (Map a b, (a, b)))
pickOne' g m
  | Map.null m = pure Nothing
  | otherwise = do
    i <- randomRM (0, Map.size m - 1) g
    let (k, v) = Map.elemAt i m
    pure $ Just (Map.deleteAt i m, (k, v))

routeBalls :: IOGenM StdGen
           -> Array (X, Y) (Vector BallType')
           -> IO (Array (X, Y) (PuzzleConnectors (Vector Int)))
routeBalls g grid = do
  routed :: IOArray (X, Y) (PuzzleConnectors (Vector Int)) <- MArray.newGenArray bounds $ const $ pure emptyConnectors
  forM_ (reverse [1..yMax]) $ \y -> do
    let
      local = concatMap (\(x, tys) -> (x,) <$> tys) $ flip fmap [0..xMax] $ \x -> (x, Vector.toList $ grid Array.! (x,y))
      above = concatMap (\(x, tys) -> (x,) <$> tys) $ flip fmap [0..xMax] $ \x -> (x, Vector.toList $ grid Array.! (x,y-1))
    paths <- pathsTo g local above
    forM_ (Map.toList paths) $ \(ty, ps) -> do
      forM_ ps $ \(start, diff) -> do
        writePath routed y ty (start, diff)
      pure ()
    pure ()
  forM_ [0..xMax] $ \x -> do
    MArray.modifyArray routed (x,yMax) $ modifyOutputs (modifyDirection DDown ((unBallType <$> grid Array.! (x,yMax)) <> ))
    MArray.modifyArray routed (x,0) $ modifyInputs (modifyDirection DUp ((unBallType <$> grid Array.! (x,0)) <> ))
  MArray.freeze routed
  where
    bounds@(_, (xMax, yMax)) = Array.bounds grid
    emptyConnectors = PuzzleConnectors emptyDirected emptyDirected
    emptyDirected = DirectedConnectors mempty mempty mempty mempty
    writePath :: IOArray (X, Y) (PuzzleConnectors (Vector Int))
                  -> Y -> BallType' -> (X, Int) -> IO ()
    writePath routed y ty (start, diff)
      | diff < 0 = do
          -- Target is on the left
          MArray.modifyArray routed (start, y) $ modifyInputs (modifyDirection DLeft (Vector.singleton (unBallType ty) <> ))
          MArray.modifyArray routed (start - 1, y) $  modifyOutputs (modifyDirection DRight (Vector.singleton (unBallType ty) <> ))
          writePath routed y ty (start - 1, diff + 1)
      | diff > 0 = do
          -- Target is on the right
          MArray.modifyArray routed (start, y) $ modifyInputs (modifyDirection DRight (Vector.singleton (unBallType ty) <> ))
          MArray.modifyArray routed (start + 1, y) $ modifyOutputs (modifyDirection DLeft (Vector.singleton (unBallType ty) <> ))
          writePath routed y ty (start + 1, diff - 1)
      | otherwise = do
        MArray.modifyArray routed (start, y) $ modifyInputs (modifyDirection DUp (Vector.singleton (unBallType ty) <> ))
        MArray.modifyArray routed (start, y - 1) $ modifyOutputs (modifyDirection DDown (Vector.singleton (unBallType ty) <> ))

fromOutputRows :: PuzzleConfig -> [PuzzleRow] -> IO (Array (X, Y) (Vector BallType'))
fromOutputRows cfg rows' = do
  connections :: IOArray (X, Y) (Vector BallType') <- MArray.newGenArray ((0,0), (Vector.length firstRow - 1, Vector.length cells - 1)) $ const $ pure Vector.empty
  Vector.forM_ (Vector.indexed cells) $ \(y, row) -> do
    Vector.forM_ (Vector.indexed row) $ \(x, cell) -> do
      MArray.writeArray connections (x, y) cell
  MArray.freeze connections
  where
    rows = Vector.fromList rows'
    firstRow = cells Vector.! 0
    cells = fmap (toCell cfg) rows

toCell :: PuzzleConfig -> PuzzleRow -> Vector (Vector BallType')
toCell cfg row = os
  where
    os :: Vector (Vector BallType')
    os = Vector.fromList $ fmap (Vector.mapMaybe toBallType) $ chunked $ unPuzzleRow row
    chunked xs
      | Vector.null xs = []
      | otherwise =
          let (a, b) = Vector.splitAt chunkSize xs
          in a : chunked b
    chunkSize = Set.size $ puzzleLocations cfg

-- connectColumns :: PuzzleConfig -> OutputColumn -> OutputColumn ->

getRelative' :: Array (X, Y) a -> (X, Y) -> Direction -> Maybe ((X, Y), a)
getRelative' g (x0, y0) r
  | Ix.inRange bounds rp = Just (rp,g Array.! rp)
  | otherwise = Nothing
  where
    rp = bimap (x0 +) (y0 +) $ relMap r
    bounds = Array.bounds g

relMap :: Direction -> (X, Y)
relMap DUp    = ( 0, -1)
relMap DLeft  = (-1,  0)
relMap DRight = ( 1,  0)
relMap DDown  = ( 0,  1)

rowIndex :: PuzzleRow -> Int -> PuzzleConnector
rowIndex p = (unPuzzleRow p Vector.!)

rowLength :: PuzzleRow -> Int
rowLength = Vector.length . unPuzzleRow

modifyPuzzleRow :: PuzzleRow -> (forall s. MVector s PuzzleConnector -> ST s ()) -> PuzzleRow
modifyPuzzleRow row f =
  PuzzleRow $ Vector.create $ do
    v <- Vector.thaw (unPuzzleRow row)
    _ <- f v
    pure v

updateRow :: PuzzleRow -> [(Int,PuzzleConnector )] -> PuzzleRow
updateRow row updates = PuzzleRow $ unPuzzleRow row Vector.// updates

findBalls :: PuzzleRow -> BallType' -> Vector Int
findBalls row b = Vector.findIndices (== ConnectorOpen b) (unPuzzleRow row)

findEmpty :: PuzzleRow -> Vector Int
findEmpty row = Vector.findIndices (== ConnectorEmpty) (unPuzzleRow row)

rowBalls :: PuzzleRow -> Vector BallType'
rowBalls = Vector.mapMaybe toBallType . unPuzzleRow

rowBallsIndexed :: PuzzleRow -> Vector (Int, BallType')
rowBallsIndexed = Vector.mapMaybe (\(i, r) -> (i, ) <$> toBallType r) . Vector.indexed . unPuzzleRow

ballCounts :: PuzzleRow -> Map BallType' Int
ballCounts row = Map.fromListWith (+) $ fmap (,1) $ Vector.toList $ rowBalls row

addBallType :: IOGenM StdGen -> PuzzleConfig -> PuzzleRow -> IO PuzzleRow
addBallType g cfg row = do
  ball <- randomBallType g cfg
  sampleOne g emptySlots >>= \case
    Nothing -> pure row
    Just i -> pure $ updateRow row [(i, ConnectorOpen ball)]
  where
    emptySlots = findEmpty row

  -- TODO this could do a weighted sample
removeRandomBall :: IOGenM StdGen -> PuzzleConfig -> PuzzleRow -> IO PuzzleRow
removeRandomBall g cfg row =
  pickOne g extras >>= \case
    Nothing -> pure row
    (Just bt) -> do
      sampleOne g (findBalls row bt) >>= \case
        Nothing -> pure row
        Just i -> pure $ updateRow row [(i, ConnectorEmpty)]
  where
    r = Vector.toList $ Vector.mapMaybe toBallType $ unPuzzleRow row
    extras =  r List.\\ (BallType' <$> minBallConfiguration cfg)

sampleOne :: IOGenM StdGen -> Vector a -> IO (Maybe a)
sampleOne g v = do
  i <- randomRM (0, Vector.length v - 1) g
  pure $ v Vector.!? i

towardsBallCount :: IOGenM StdGen -> PuzzleConfig -> Int -> PuzzleRow -> IO PuzzleRow
towardsBallCount g cfg ballCount row
  | Vector.length balls < ballCount = do
    putStrLn "Adding ball"
    addBallType g cfg row
  | Vector.length balls > ballCount = do
    putStrLn "Removing ball"
    removeRandomBall g cfg row
  | otherwise = pure row
  where
    balls = rowBalls row

spaceOutputs :: PuzzleConfig -> PuzzleRow -> IO PuzzleRow
spaceOutputs cfg row = do
  -- (0,10,50,5,10)
  print (outsideSpace, ballSpace, rowLen, locationCount, ballCount)

  pure $ PuzzleRow $ Vector.create $ do
    v <- MVector.generate rowLen $ const ConnectorEmpty
    forM_ [0 .. ballCount - 1] $ \i -> do
      MVector.write v ((locationCount`div` 2) + i * ballSpace) $ ConnectorOpen $ balls Vector.! i
    pure v
  where
    outsideSpace = (rowLen `mod` locationCount) `div` 2
    ballSpace = rowLen `div` ballCount
    locationCount = Set.size $ puzzleLocations cfg
    rowLen = rowLength row
    ballCount = Vector.length balls
    balls = Vector.mapMaybe toBallType $ unPuzzleRow row

data SwapConfig = SwapConfig {
    swapRadius :: (Int, Int)
  , ballSwaps  :: (Int, Int)
} deriving (Eq, Ord, Show)

swapBalls :: IOGenM StdGen -> SwapConfig -> PuzzleRow -> IO PuzzleRow
swapBalls g swaps row = do
  swapCount <- randomRM (ballSwaps swaps) g
  toSwap <- pickN g (Set.fromList [0.. len -1]) swapCount
  shuffles <- forM (Set.toList toSwap) $ \start -> do
    radius <- randomRM (swapRadius swaps) g
    r <- randomRM (-radius, radius) g
    let i = min (len - 1) $ max 0 $ start + r
    pure $ (start,i)
  pure $ modifyPuzzleRow row $ \v -> do
    forM_ shuffles $ \(i, j) -> do
      let
        (iB, _) = balls Vector.! i
        (jB, _) = balls Vector.! j
      MVector.swap v iB jB
  where
    len = Vector.length balls
    balls = rowBallsIndexed row

renderRows :: [PuzzleRow] -> String
renderRows = unlines . fmap (List.intersperse ' ' . Vector.toList . Vector.map renderConnector . unPuzzleRow)

renderConnector :: PuzzleConnector -> Char
renderConnector ConnectorEmpty                = ' '
renderConnector (ConnectorOpen (BallType' i)) = head $ show i

applyAll :: forall a. a -> [a -> IO a] -> IO [a]
applyAll a fs =
  fmap (uncurry (:)) $ Foldable.foldlM (\(x,xs) f -> f x >>= \x' -> pure (x',x:xs)) (a, []) fs

-- swapBalls ::

-- sortTo :: PuzzleRow -> PuzzleRow -> IO PuzzleRow
-- sortTo target row = do
--   let targetBalls = rowBalls target
--       rowBalls' = rowBalls row
--       targetBalls' = Vector.filter (`notElem` rowBalls') targetBalls
--   pure $ foldl' (\r b -> updateRow r [(i, ConnectorOpen b)]) row $ Vector.toList targetBalls'
--   where
--     i = Vector.length (unPuzzleRow row) - 1

-- max swaps is the number of locations

data Direction = DUp | DDown | DLeft | DRight
  deriving (Eq, Ord, Show)

directions :: [Direction]
directions = [DUp, DDown, DLeft, DRight]

dirToRel :: Direction -> RelativeCell
dirToRel DUp    = RCUp
dirToRel DDown  = RCDown
dirToRel DLeft  = RCLeft
dirToRel DRight = RCRight

onEdge :: Direction -> Double -> Position
onEdge DUp x    = (x, 0)
onEdge DDown x  = (x, 1)
onEdge DLeft y  = (0, y)
onEdge DRight y = (1, y)

switchDirection :: Direction -> Direction
switchDirection DUp    = DDown
switchDirection DDown  = DUp
switchDirection DLeft  = DRight
switchDirection DRight = DLeft

-- | Connectors for a puzzle for each edge of the puzzle
data DirectedConnectors i = DirectedConnectors {
  dcUp    :: i
, dcDown  :: i
, dcLeft  :: i
, dcRight :: i
} deriving (Eq, Ord, Show)

instance Functor DirectedConnectors where
  fmap f (DirectedConnectors u d l r) = DirectedConnectors (f u) (f d) (f l) (f r)

-- | Get the connectors in a direction
inDirection :: Direction -> DirectedConnectors i -> i
inDirection DUp    = dcUp
inDirection DDown  = dcDown
inDirection DLeft  = dcLeft
inDirection DRight = dcRight

-- | Modify the connectors in a direction
modifyDirection :: Direction -> (i -> i) -> DirectedConnectors i -> DirectedConnectors i
modifyDirection DUp f (DirectedConnectors u d l r) = DirectedConnectors (f u) d l r
modifyDirection DDown f (DirectedConnectors u d l r) = DirectedConnectors u (f d) l r
modifyDirection DLeft f (DirectedConnectors u d l r) = DirectedConnectors u d (f l) r
modifyDirection DRight f (DirectedConnectors u d l r) = DirectedConnectors u d l (f r)

-- | Turn the connectors into a map of directions to connectors
directed :: DirectedConnectors i -> Map Direction i
directed d = Map.fromList $ (id &&& flip inDirection d) <$> directions

-- | Sum of connectors in all directions
connectorCount :: Num i => DirectedConnectors i -> i
connectorCount (DirectedConnectors u d l r) = u + d + l + r

-- | The inputs and outputs of a puzzle
data PuzzleConnectors i = PuzzleConnectors {
  puzzleInputs  :: DirectedConnectors i
, puzzleOutputs :: DirectedConnectors i
} deriving (Eq, Ord, Show)

instance Functor PuzzleConnectors where
  fmap f (PuzzleConnectors i o) = PuzzleConnectors (fmap f i) (fmap f o)

usedSides :: PuzzleConnectors Int -> Int
usedSides (PuzzleConnectors i o) = Map.size $ Map.filter (> 0) $ Map.unionWith (+) (directed i) (directed o)

-- | Modify the input connectors of a puzzle
modifyInputs :: (DirectedConnectors i -> DirectedConnectors i) -> PuzzleConnectors i -> PuzzleConnectors i
modifyInputs f (PuzzleConnectors i o) = PuzzleConnectors (f i) o

-- | Modify the output connectors of a puzzle
modifyOutputs :: (DirectedConnectors i -> DirectedConnectors i) -> PuzzleConnectors i -> PuzzleConnectors i
modifyOutputs f (PuzzleConnectors i o) = PuzzleConnectors i (f o)

genPuzzleMachine :: [BallTypes] -> X -> Y -> MetaMachine Puzzle
genPuzzleMachine btys xdim ydim = do
    let
      (ballPit, _, _) =
        let
          l = length btys
          (q, r) = l `quotRem` xdim
        in foldl'
           (\(m, a, btys') i ->
              let (h, t)
                    | a == 0 = splitAt q btys'
                    | otherwise = splitAt (q+1) btys'
              in (Map.insert i h m, if a == 0 then 0 else a-1, t))
           (Map.empty, r, btys)
           [0..xbound]

      arr = Array.array ((0,0), (xbound, ybound))
        [( (x,y)
         , let
             -- invariant: if an entry exists in the ballpit, then bls is at least 1.
             -- If not, this is an error in the quotient code (qMap).
             xbls = case Map.lookup x ballPit of
               Nothing -> error "genPuzzleMachine: Ball entry empty"
               Just bs -> length bs

             -- invariant: normalize the ball position before inserting
             -- into puzzles so that the relative position is biased towards
             -- the start of the cell (not the ball)
             ds = fromList $
               [ (fromIntegral i + 0.5) / fromIntegral xbls
               | i <- [0..xbls - 1]
               ]

             -- invariant: fill in the first row with the simplest machine:
             -- a straight down path req.
             reqTiles = fromList [RCUp | y /= 0]
           in Puzzle reqTiles (fmap (\d -> Gateway (d,0) (fromList [GatewayBall 1 1]) ) ds) (fmap (\d -> Gateway (d,1) (fromList [GatewayBall 1 1])) ds) mempty
         )

        | x <- [0..xbound]
        , y <- [0..ybound]
        ]

    MetaMachine arr (TileSize 700 700) 0.001 mempty
  where
    xbound :: X
    xbound = xdim - 1

    ybound :: Y
    ybound = ydim - 1
