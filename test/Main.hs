{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Control.Concurrent.Async
import           Control.Monad
import qualified Control.Monad.Catch as E
import           Control.Monad.Extra (whileM)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.Aeson as JS
import qualified Data.Array as Array
import           Data.Cache.LRU.IO (AtomicLRU)
import qualified Data.Cache.LRU.IO as LRU
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List (nub, sort, isSubsequenceOf)
import           Data.Time
import           Data.Time.Calendar.OrdinalDate
import           Data.UUID
import qualified Data.UUID.Types.Internal as UUID
import qualified Database.Redis as Redis
import           GHC.IsList
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Incredible.Config
import           Incredible.Data
import           Incredible.DataStore
import qualified Incredible.DataStore.Memory as MemStore
import qualified Incredible.DataStore.Redis as IRedis
import           Incredible.Puzzle
import           Test.Tasty
import qualified Test.Tasty.Hedgehog as H
import           System.Directory
import           System.IO (hGetLine)
import           System.IO.Temp (emptySystemTempFile)
import qualified System.Process as Process

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Incredible"
  [ toFromJSONTests
  , genPuzzleTests
  , checkReady
  , checkRemanufacture
  , deltaMachineTest
  , checkDataStore "Memory Store" (\mp act -> act =<< fst <$> MemStore.initIncredibleState () mp)
  , checkDataStore "Redis Store" testingRedis
  ]

toJSObject :: JS.ToJSON a => a -> JS.Object
toJSObject a = case JS.toJSON a of
  JS.Object o -> o
  _ -> error "Wasn't actually a JSON Object."

genPosition :: MonadGen m => m Position
genPosition = (,) <$> Gen.realFloat (Range.constant 0 1) <*> Gen.realFloat (Range.constant 0 1)

genGateway :: MonadGen m => m Gateway
genGateway = Gateway <$> genPosition <*> (fromList <$> Gen.list (Range.constant 0 5) (GatewayBall <$> (Gen.integral (Range.constant 1 4)) <*> Gen.realFloat (Range.constant 0 1)))

genPuzzle :: (MonadGen m, JS.ToJSON a) => a -> m Puzzle
genPuzzle p =
  Puzzle
    <$> (fromList <$> Gen.list (Range.constant 0 5) Gen.enumBounded)
    <*> (fromList <$> Gen.list (Range.constant 0 5) genGateway)
    <*> (fromList <$> Gen.list (Range.constant 0 5) genGateway)
    <*> pure (toJSObject p)

genUUID :: MonadGen m => m UUID
genUUID = do
  b0 <- Gen.word8 Range.constantBounded
  b1 <- Gen.word8 Range.constantBounded
  b2 <- Gen.word8 Range.constantBounded
  b3 <- Gen.word8 Range.constantBounded
  b4 <- Gen.word8 Range.constantBounded
  b5 <- Gen.word8 Range.constantBounded
  b6 <- Gen.word8 Range.constantBounded
  b7 <- Gen.word8 Range.constantBounded
  b8 <- Gen.word8 Range.constantBounded
  b9 <- Gen.word8 Range.constantBounded
  ba <- Gen.word8 Range.constantBounded
  bb <- Gen.word8 Range.constantBounded
  bc <- Gen.word8 Range.constantBounded
  bd <- Gen.word8 Range.constantBounded
  be <- Gen.word8 Range.constantBounded
  bf <- Gen.word8 Range.constantBounded
  pure $ UUID.buildFromBytes 4 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf

genTime :: MonadGen m => m UTCTime
genTime =
  UTCTime
    <$> (fromOrdinalDate <$> Gen.integral (Range.linearFrom 2024 (-100) 3000)
    <*> Gen.int (Range.constant 1 366))
    <*> Gen.realFrac_ (Range.constant 0 86401)

genBlueprint :: (MonadGen m, JS.ToJSON a) => HashMap Int a -> m Blueprint
genBlueprint s =
  Blueprint
    <$> genUUID
    <*> Gen.text (Range.constant 0 50) Gen.unicode
    <*> Gen.choice [Just <$> genTime, pure Nothing]
    <*> pure (fmap toJSObject s)

genMetaMachine :: (MonadGen m, JS.ToJSON tile) => (MonadGen m => (X, Y) -> m tile) -> m (MetaMachine tile)
genMetaMachine tileGen = do
  xsize <- Gen.integral (Range.exponential 1 100)
  ysize <- Gen.integral (Range.exponential 1 100)
  genMetaMachineSized xsize ysize tileGen

genMetaMachineSized :: (MonadGen m, JS.ToJSON tile) => X -> Y -> (MonadGen m => (X, Y) -> m tile) -> m (MetaMachine tile)
genMetaMachineSized xsize ysize tileGen = do
  ts <- TileSize <$> Gen.int (Range.constant 10 1000) <*> Gen.int (Range.constant 10 1000)
  r <- Gen.realFrac_ (Range.constant 0 1)
  pp <- (fromList <$> Gen.list (Range.constant 0 5) genUUID)
  translateMachine (\xy _ -> tileGen xy) $ MetaMachine (Array.listArray ((0,0), (xsize-1, ysize-1)) [()..]) ts r pp

toFromJSONTests :: TestTree
toFromJSONTests = testGroup "toFromJSON"
  [ toFromRelativeCellTest
  , toFromPuzzleTest
  , toFromBlueprint
  , toFromMetaMachinePuzzleID
  , toFromGameState
  ]
  where
    checkJSON :: (JS.ToJSON a, JS.FromJSON a, Eq a, Show a, MonadTest m) => a -> m ()
    checkJSON v = do
      JS.Success v === JS.fromJSON (JS.toJSON v)
      Just v === JS.decode (JS.encode v)
    toFromRelativeCellTest = H.testProperty "RelativeCell To/From Inverse" $ property $ do
      rc::RelativeCell <- forAll Gen.enumBounded
      checkJSON rc
    toFromPuzzleTest = H.testProperty "Puzzle To/From Inverse" $ property $ do
      unitPzl <- forAll $ genPuzzle $ JS.object []
      strPzl  <- forAll $ genPuzzle $ JS.object ["str" JS..= ("test string"::String)]
      arrPzl  <- forAll $ genPuzzle $ JS.object ["one" JS..= (1::Int), "two" JS..= (2::Int)]
      checkJSON unitPzl
      checkJSON strPzl
      checkJSON arrPzl
    toFromBlueprint = H.testProperty "Blueprint To/From Inverse" $ property $ do
      unitBp <- forAll $ genBlueprint $ (mempty::HashMap Int JS.Object)
      strBp  <- forAll $ genBlueprint $ HashMap.singleton 1 $ JS.object ["str" JS..= ("test string"::String)]
      arrBp  <- forAll $ genBlueprint $ HashMap.singleton 1 $ JS.object ["one" JS..= (1::Int), "two" JS..= (2::Int)]
      checkJSON unitBp
      checkJSON strBp
      checkJSON arrBp
    toFromMetaMachinePuzzleID = H.testProperty "MetaMachine PuzzleID To/From Inverse" $ property $ do
      mm::MetaMachine PuzzleID <- forAll $ genMetaMachine (\_ -> fmap puzzleID $ genPuzzle $ JS.object [])
      checkJSON mm
    toFromGameState = H.testProperty "GameState PuzzleID To/From Inverse" $ property $ do
      mm::GameState <- forAll $ genMetaMachine (\_ -> Gen.choice [ fmap (Left . puzzleID)     $ genPuzzle    $ JS.object []
                                                                 , fmap (Right . blueprintID) $ genBlueprint $ HashMap.singleton 1 $ JS.object []
                                                                 ])
      checkJSON mm

baseMeta :: Array.Array (Int, Int) a -> MetaMachine a
baseMeta a = MetaMachine a (TileSize 700 700) 0.001 mempty

genPuzzleTests :: TestTree
genPuzzleTests = H.testProperty "Puzzle Generator" $ property $ test $ do
      let m0 = genPuzzleMachine [BallType '0'] 1 1
      m0 === baseMeta (Array.array ((0,0), (0,0)) [((0, 0), Puzzle [] [Gateway (0.5, 0) [GatewayBall 1 1]] [Gateway (0.5, 1) [GatewayBall 1 1]] mempty)])
      let m1 = genPuzzleMachine [BallType '0', BallType '1'] 1 1
      m1 === baseMeta (Array.array ((0,0), (0,0)) [((0, 0), Puzzle [] [Gateway  (0.25, 0) [GatewayBall 1 1], Gateway (0.75, 0) [GatewayBall 1 1]] [Gateway (0.25, 1) [GatewayBall 1 1], Gateway  (0.75, 1) [GatewayBall 1 1]] mempty)])
      let m2 = genPuzzleMachine [BallType '0', BallType '1'] 2 1
      m2 === baseMeta (Array.array ((0,0), (1,0)) [ ((0, 0), Puzzle [] [Gateway (0.5, 0) [GatewayBall 1 1]] [Gateway  (0.5, 1) [GatewayBall 1 1]] mempty)
                                                       , ((1, 0), Puzzle [] [Gateway (0.5, 0) [GatewayBall 1 1]] [Gateway (0.5, 1) [GatewayBall 1 1]] mempty)
                                                       ])
      let m3 = genPuzzleMachine [BallType '0', BallType '1'] 2 1
      m3 === baseMeta (Array.array ((0,0), (1,0)) [ ((0, 0), Puzzle [] [Gateway (0.5, 0) [GatewayBall 1 1]] [Gateway (0.5, 1) [GatewayBall 1 1]] mempty)
                                                       , ((1, 0), Puzzle [] [Gateway (0.5, 0) [GatewayBall 1 1]] [Gateway (0.5, 1) [GatewayBall 1 1]] mempty)
                                                       ])
      let m4 = genPuzzleMachine [BallType '0', BallType '1'] 2 2
      m4 === baseMeta (Array.array ((0,0), (1,1)) [ ((0, 0), Puzzle [] [Gateway (0.5, 0) [GatewayBall 1 1]] [Gateway (0.5, 1) [GatewayBall 1 1]] mempty)
                                                       , ((1, 0), Puzzle [] [Gateway (0.5, 0) [GatewayBall 1 1]] [Gateway (0.5, 1) [GatewayBall 1 1]] mempty)
                                                       , ((0, 1), Puzzle [RCUp] [Gateway (0.5, 0) [GatewayBall 1 1]] [Gateway (0.5, 1) [GatewayBall 1 1]] mempty)
                                                       , ((1, 1), Puzzle [RCUp] [Gateway (0.5, 0) [GatewayBall 1 1]] [Gateway (0.5, 1) [GatewayBall 1 1]] mempty)
                                                       ])
      let m5 = genPuzzleMachine [BallType '0', BallType '1', BallType '2'] 2 1
      m5 === baseMeta (Array.array ((0,0), (1,0)) [ ((0, 0), Puzzle [] [Gateway (0.25, 0) [GatewayBall 1 1], Gateway (0.75,0) [GatewayBall 1 1]] [Gateway  (0.25, 1) [GatewayBall 1 1], Gateway (0.75, 1) [GatewayBall 1 1]] mempty)
                                                       , ((1, 0), Puzzle [] [Gateway (0.5, 0) [GatewayBall 1 1]] [Gateway (0.5, 1) [GatewayBall 1 1]] mempty)
                                                       ])

      let m6 = genPuzzleMachine [BallType '0', BallType '1'] 1 2
      m6 === baseMeta (Array.array ((0,0), (0,1)) [ ((0, 0), Puzzle [] [Gateway (0.25,0) [GatewayBall 1 1], Gateway (0.75,0) [GatewayBall 1 1]] [Gateway (0.25,1) [GatewayBall 1 1], Gateway (0.75,1) [GatewayBall 1 1]] mempty)
                                                       , ((0, 1), Puzzle [RCUp] [Gateway (0.25,0) [GatewayBall 1 1], Gateway (0.75,0) [GatewayBall 1 1]] [Gateway (0.25,1) [GatewayBall 1 1], Gateway (0.75,1) [GatewayBall 1 1]] mempty)
                                                       ])
checkReady :: TestTree
checkReady = H.testProperty "Check Ready Puzzles" $ property $ test $ do
  let pzl0 = Puzzle [] [Gateway  (0.5, 0) [GatewayBall 1 1]] [Gateway (0.5, 1) [GatewayBall 1 1]] mempty
  let pzl1 = Puzzle [RCUp] [Gateway (0.5, 0) [GatewayBall 1 1]] [Gateway (0.5, 1) [GatewayBall 1 1]] mempty
  let pzlMap = HashMap.fromList [(puzzleID pzl0, pzl0), (puzzleID pzl1, pzl1)]
  let m0 = baseMeta $ Array.array ((0,0), (1,1)) [ ((0, 0), Left $ puzzleID pzl0)
                                                    , ((1, 0), Left $ puzzleID pzl0)
                                                    , ((0, 1), Left $ puzzleID pzl1)
                                                    , ((1, 1), Left $ puzzleID pzl1)
                                                    ]
  [pWithID pzl0, pWithID  pzl0] === findReadyPuzzles pzlMap m0
  let bp0 = Blueprint (puzzleID pzl0) "" Nothing mempty
  let m1 = baseMeta $ Array.array ((0,0), (1,1)) [ ((0, 0), Left $ puzzleID pzl0)
                                                 , ((1, 0), Right $ blueprintID bp0)
                                                 , ((0, 1), Left $ puzzleID pzl1)
                                                 , ((1, 1), Left $ puzzleID pzl1)
                                                 ]
  [pWithID pzl0, pWithID pzl1] === findReadyPuzzles pzlMap m1
  where
    pWithID pzl = (puzzleID pzl, pzl)

checkRemanufacture :: TestTree
checkRemanufacture = H.testProperty "remanufacture" $ property $ test $ do
  let pzl0 = Puzzle [] [Gateway (0.5, 0) [GatewayBall 1 1]] [Gateway (0.5, 1) [GatewayBall 1 1]] mempty
  let pzl1 = Puzzle [RCUp] [Gateway (0.5, 0) [GatewayBall 1 1]] [Gateway (0.5, 1) [GatewayBall 1 1]] mempty
  let m0 = baseMeta $ Array.array ((0,0), (1,1)) [ ((0, 0), pzl0)
                                                    , ((1, 0), pzl0)
                                                    , ((0, 1), pzl1)
                                                    , ((1, 1), pzl1)
                                                    ]
  fmap (Left . puzzleID) m0 === remanufacture m0 (fmap (Left . puzzleID) m0)
  let m1 = baseMeta $ Array.array ((0,0), (1,1)) [ ((0, 0), pzl0)
                                                    , ((1, 0), pzl0)
                                                    , ((0, 1), pzl1)
                                                    , ((1, 1), Puzzle [RCUp] [Gateway (0.5, 0) [GatewayBall 1 1]] [Gateway (1, 1) [GatewayBall 1 1]] mempty)
                                                    ]
  fmap (Left . puzzleID) m1 === remanufacture m1 (fmap (Left . puzzleID) m0)
  let m2 = baseMeta $ Array.array ((0,0), (1,1)) [ ((0, 0), Left $ puzzleID pzl0)
                                                    , ((1, 0), Left $ puzzleID pzl0)
                                                    , ((0, 1), Left $ puzzleID pzl1)
                                                    , ((1, 1), Right $ Blueprint (puzzleID pzl1) "" Nothing mempty)
                                                    ]
  fmap (Left . puzzleID) m1 === remanufacture m1 m2
  let bp1 = Blueprint (puzzleID pzl1) "" Nothing mempty
  let m3 = baseMeta $ Array.array ((0,0), (1,1)) [ ((0, 0), Left $ puzzleID pzl0)
                                                    , ((1, 0), Left $ puzzleID pzl0)
                                                    , ((0, 1), Right bp1)
                                                    , ((1, 1), Right $ Blueprint (puzzleID pzl1) "" Nothing mempty)
                                                    ]
  baseMeta (fmap (Left . puzzleID) (mmGrid m1) Array.// [((0,1), Right $ blueprintID bp1)]) === remanufacture m1 m3
  let m4 = baseMeta $ Array.array ((0,0), (2,1)) [ ((0, 0), pzl0)
                                                 , ((1, 0), pzl0)
                                                 , ((2, 0), pzl0)
                                                 , ((0, 1), pzl1)
                                                 , ((1, 1), pzl1)
                                                 , ((2, 1), pzl1)
                                                 ]
  (fmap (Left . puzzleID) m4) === remanufacture m4 (fmap (Left . puzzleID) m0)
  let m5 = baseMeta $ Array.array ((0,0), (1,1)) [ ((0, 0), Left $ puzzleID pzl0)
                                                 , ((1, 0), Left $ puzzleID pzl0)
                                                 , ((0, 1), Right bp1)
                                                 , ((1, 1), Left $ puzzleID pzl1)
                                                 ]
  let m5r' = fmap (Left . puzzleID) m4
  let m5r = m5r' { mmGrid = (mmGrid m5r') Array.// [((0,1), Right $ blueprintID bp1)] }
  m5r === remanufacture m4 m5

deltaMachineTest :: TestTree
deltaMachineTest = H.testProperty "deltaMachine" $ property $ do
  -- xsize <- forAll $ Gen.integral (Range.exponential 1 100)
  -- ysize <- forAll $ Gen.integral (Range.exponential 1 100)
  ma <- forAll $ genMetaMachine $ const $ Gen.word8 Range.constantBounded
  mb <- forAll $ genMetaMachine $ const $ Gen.word8 Range.constantBounded
  let da2b = ma `deltaMachine` mb
  let db2a = mb `deltaMachine` ma
  mb === applyDelta ma da2b
  ma === applyDelta mb db2a

data StoreReader s
  = StoreReader
    { srStore :: s
    , srMC :: AtomicLRU MachineVersion GameState
    , srBC :: AtomicLRU BlueprintID Blueprint
    , srSC :: AtomicLRU BlueprintID Snapshot
    }

instance IncredibleData s => HasDataStore (StoreReader s) s where
  getStore = srStore
  getMachineCache = srMC
  getBlueprintCache = srBC
  getSnapshotCache = srSC

testingRedis :: (MonadFail m', MonadIO m', E.MonadCatch m') => MetaMachine Puzzle -> (IRedis.IncredibleRedisStore -> m' a) -> m' a
testingRedis mp act = do
  -- PropertyT is not MonadMask
  unixSocketPath <- liftIO $ emptySystemTempFile "redis.socket"
  liftIO $ removeFile unixSocketPath
  let c = (Process.proc "redis-server"
                        [ "--unixsocket", unixSocketPath
                        , "--port", "0"
                        , "--bind", "127.0.0.1"
                        , "--unixsocketperm", "700"
                        , "--maxmemory-policy", "volatile-ttl"
                        , "--maxmemory", "100mb"
                        , "--maxclients", "100"
                        , "--appendonly", "no"
                        , "--save", ""
                        , "--loglevel", "notice"
                        ])
            { Process.std_in = Process.Inherit
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.Inherit
            , Process.close_fds = True
            }
  p@(_, Just hout, _, _) <- liftIO $ Process.createProcess c
  (`E.onException` (liftIO $ Process.cleanupProcess p)) $ do
    -- Wait for redis to say it is ready
    whileM $ liftIO $ do
        rol <- hGetLine hout
        pure $ not $ "Ready to accept connections unix" `isSubsequenceOf` rol
    -- Might need to keep consuming the stdout to avoid blocking.
    let rc = Just $ IncredibleRedisConfig {
                    incredibleRedisHostName = ""
                  , incredibleRedisPort = Redis.UnixSocket unixSocketPath
                  , incredibleRedisDatabase = 0
                  , incredibleRedisMaxConnections = 100
                  , incredibleRedisMaxIdleTimeout = 10
                  , incredibleRedisPassword = Nothing
                  , incredibleRedisRetryCount = 10
                  , incredibleWorkOrderTTL = 1
                  , incredibleOrderBookTTL = 1
                  , incredibleRedisUseTLS = False
                }
    r' <- act =<< fst <$> IRedis.initIncredibleState (IncredibleConfig undefined undefined rc undefined) mp
    liftIO $ Process.cleanupProcess p
    pure r'

-- The storer needs to be seperated to different datasets.
checkDataStore :: forall s . IncredibleData s => String -> (forall m' a . (MonadFail m', MonadIO m', E.MonadCatch m') => MetaMachine Puzzle -> (s -> m' a) -> m' a) -> TestTree
checkDataStore storeName storer = testGroup storeName
  [ checkDBStart
  , checkEdit
  , checkConcurrent
  , checkAddBlueprint
  , checkMod
  ]
  where
    mkStore :: (MonadFail m, MonadIO m, E.MonadCatch m) => MetaMachine Puzzle -> (StoreReader s -> m a) -> m a
    mkStore mp act = storer mp $ \s ->
      (StoreReader s <$> liftIO (LRU.newAtomicLRU (Just 1))) <*> liftIO (LRU.newAtomicLRU (Just 1)) <*> liftIO (LRU.newAtomicLRU (Just 1)) >>= act
    checkDBStart = H.testProperty "DB Start" $ property $ do
      mm0 <- forAll $ genMetaMachine (const $ genPuzzle $ JS.object [])
      mkStore mm0 $ \sr -> do
        sm0 <- runReaderT getCurrentMachine sr
        VersionedMachine 0 (fmap (Left . puzzleID) mm0) === sm0
        sm1 <- runReaderT (getMachine 1) sr
        Nothing === sm1
    checkEdit = H.testProperty "Edit" $ property $ do
      xsize <- forAll $ Gen.integral (Range.linear 1 10)
      ysize <- forAll $ Gen.integral (Range.linear 1 10)
      ma <- forAll $ genMetaMachineSized xsize ysize $ const $ genPuzzle $ JS.object ["value" JS..= 'a']
      mb <- forAll $ genMetaMachineSized xsize ysize $ const $ genPuzzle $ JS.object ["value" JS..= 'b']
      assert $ ma /= mb
      mkStore ma $ \sr -> (`runReaderT` sr) $ do
        me0 <- getCurrentMachine
        VersionedMachine 0 (fmap (Left . puzzleID) ma) === me0
        editCurrentMachine (const ((), fmap (Left . puzzleID) ma))
        me1 <- getCurrentMachine
        VersionedMachine 0 (fmap (Left . puzzleID) ma) === me1
        editCurrentMachine (const ((), fmap (Left . puzzleID) mb))
        me2 <- getCurrentMachine
        VersionedMachine 1 (fmap (Left . puzzleID) mb) === me2
        editCurrentMachine (const ((), fmap (Left . puzzleID) mb))
        me3 <- getCurrentMachine
        VersionedMachine 1 (fmap (Left . puzzleID) mb) === me3
        editCurrentMachine (const ((), fmap (Left . puzzleID) ma))
        me4 <- getCurrentMachine
        VersionedMachine 2 (fmap (Left . puzzleID) ma) === me4
    checkConcurrent = H.testProperty "Concurrent" $ property $ do
      xsize <- forAll $ Gen.integral (Range.linear 1 10)
      ysize <- forAll $ Gen.integral (Range.linear 1 10)
      ma <- forAll $ genMetaMachineSized xsize ysize $ const $ genPuzzle $ JS.object ["value" JS..= (0::Int)]
      let raceSize = (10::Int)
      mbs <- mapM (\mv -> forAll $ genMetaMachineSized xsize ysize $ const $ genPuzzle $ JS.object ["value" JS..= mv]) [1..raceSize]
      assert $ length mbs == raceSize
      assert $ notElem ma mbs
      assert $ length (nub mbs) == raceSize
      mkStore ma $ \sr -> do
        let machineList = fmap (fmap (Left . puzzleID)) $ ma:mbs
        let updateMap::HashMap MachineVersion GameState = HashMap.fromList $ zip [0..] $ tail machineList
        updates <- liftIO $ forM mbs $ \_ -> async $ (`runReaderT` sr) $ editCurrentMachine $ \gs -> ((), updateMap HashMap.! vmVersion gs)
        mapM_ (liftIO . wait) updates
        forM_ (zip [0..] machineList) $ \(i, mm) -> do
          mmi <- (`runReaderT` sr) $ getMachine i
          Just mm === mmi
        lm <- (`runReaderT` sr) getCurrentMachine
        VersionedMachine (fromIntegral raceSize) (last machineList) === lm
    checkAddBlueprint = H.testProperty "Add Blueprint" $ property $ do
      let mm = genPuzzleMachine [BallType '0'] 1 1
      bpCount <- forAll $ Gen.int (Range.linear 1 20)
      bps <- replicateM bpCount $ forAll $ genBlueprint (mempty::HashMap Int JS.Object)
      mkStore mm $ \sr -> do
        (`runReaderT` sr) $ traverse_ queueModeration bps
        (`runReaderT` sr) $ forM_ bps $ \bp -> do
          gbp <- getBlueprint $ blueprintID bp
          Just bp === gbp
        (`runReaderT` sr) $ do
          gbp0 <- getBlueprint $ blueprintID $ head bps
          Just (head bps) === gbp0
          gbp1 <- getBlueprint $ blueprintID $ head bps
          Just (head bps) === gbp1
    checkMod = H.testProperty "Mod Ops" $ property $ do
      let mm = genPuzzleMachine [BallType '0'] 1 2
      bpCount <- forAll $ Gen.int (Range.linear 1 20)
      let pzlID0 = puzzleID $ mmGrid mm Array.! (0,0)
      let pzlID1 = puzzleID $ mmGrid mm Array.! (0,1)
      bps0 <- fmap (nub . fmap (\bp -> bp {bPuzzleID=pzlID0})) $ replicateM bpCount $ forAll $ genBlueprint (mempty::HashMap Int JS.Object)
      bps1 <- fmap (nub . fmap (\bp -> bp {bPuzzleID=pzlID1})) $ replicateM bpCount $ forAll $ genBlueprint (mempty::HashMap Int JS.Object)
      mkStore mm $ \sr -> do
        (`runReaderT` sr) $ forM_ ([pzlID0, pzlID1]::[PuzzleID]) $ \pid -> do
          mq <- listModerationQueue pid
          [] === mq
        (`runReaderT` sr) $ forM_ (bps0<>bps1) $ \bp -> queueModeration bp
        (`runReaderT` sr) $ forM_ ([(pzlID0, bps0), (pzlID1, bps1)]::[(PuzzleID, [Blueprint])]) $ \(pid, bps) -> do
          mq <- listModerationQueue pid
          sort (fmap blueprintID bps) === sort mq
        (`runReaderT` sr) $ forM_ (bps0<>bps1) $ \bp -> dequeueModeration (blueprintID bp)
        (`runReaderT` sr) $ forM_ ([pzlID0, pzlID1]::[PuzzleID]) $ \pid -> do
          mq <- listModerationQueue pid
          [] === mq
