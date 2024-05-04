{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# language ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# language OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Incredible.Data
 ( type PuzzleID
 , type BlueprintID
 , type X
 , type Y
 , type UserDigest
 , type UserName
 , type GameState
 , type VersionedGameState
 , type Position
 , puzzleID, blueprintID
 , RelativeCell(..)
 , Puzzle(..)
 , MetaMachine(..)
 , Blueprint(..)
 , MachineVersion
 , VersionedMachine(..)
 , MachineShop(..)
 , ModData(..)
 , BallTypes(..)
 , Gateway(..)
 , GatewayBall(..)
 , InspectionReport(..)
 , Snapshot
 , Folio(..)
 , MachineUpdates(..)
 , TileSize(..)
 , WorkOrderID(..)
 , WorkOrder(..)
 , WidgetSignature(..)
-- , ServerSharedState(..)
 , deltaMachine
 , applyDelta
 , translateMachine
 , findReadyPuzzles
 , gridizeArray
 , remanufacture
 , cachePuzzleLoc
 , isSolved
 , isSolvable
 , firstPuzzles
 , bpidsInMachine
 ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Aeson qualified as JS
import Data.Aeson.Types qualified as JS
import Data.Array (Array)
import Data.Array qualified as Array
import Data.Bifunctor (bimap, first)
import Data.Bytes.Serial qualified as S
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Cache.LRU.IO (AtomicLRU)
import Data.Data (Typeable)
import Data.Either
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List
import Data.Ix (Ix)
import Data.Ix qualified as Ix
import Data.Maybe
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Tuple
import Data.UUID (UUID)
import Data.UUID.V5 qualified as UUIDV5
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as UV
import GHC.Generics

type X = Int
type Y = Int
type PuzzleID = UUID
type BlueprintID = UUID
type MachineVersion = Integer
type Position = (Double, Double)

type GameState = MetaMachine (Either PuzzleID BlueprintID)
type VersionedGameState = VersionedMachine (Either PuzzleID BlueprintID)

type UserName = Text
type UserDigest = Text

newtype WorkOrderID
  = WorkOrderID UUID
  deriving (Show, Eq, Ord, Typeable, Generic)

instance JS.ToJSON   WorkOrderID
instance JS.FromJSON WorkOrderID

data WorkOrder
  = WorkOrder
  deriving (Show, Eq, Ord, NFData, Typeable, Generic)

instance JS.ToJSON   WorkOrder
instance JS.FromJSON WorkOrder

data RelativeCell
 = RCUpLeft
 | RCUp
 | RCUpRight
 | RCLeft
 | RCRight
 | RCDownLeft
 | RCDown
 | RCDownRight
 deriving (Read, Show, Eq, Ord, Bounded, Enum, NFData, Generic, Typeable)

instance JS.ToJSON RelativeCell where
  toJSON RCUpLeft    = "UpLeft"
  toJSON RCUp        = "Up"
  toJSON RCUpRight   = "UpRight"
  toJSON RCLeft      = "Left"
  toJSON RCRight     = "Right"
  toJSON RCDownLeft  = "DownLeft"
  toJSON RCDown      = "Down"
  toJSON RCDownRight = "DownRight"
  toEncoding RCUpLeft    = "UpLeft"
  toEncoding RCUp        = "Up"
  toEncoding RCUpRight   = "UpRight"
  toEncoding RCLeft      = "Left"
  toEncoding RCRight     = "Right"
  toEncoding RCDownLeft  = "DownLeft"
  toEncoding RCDown      = "Down"
  toEncoding RCDownRight = "DownRight"

instance JS.FromJSON RelativeCell where
  parseJSON = JS.withText "RelativeCell" $ \case
    "UpLeft"    -> pure RCUpLeft
    "Up"        -> pure RCUp
    "UpRight"   -> pure RCUpRight
    "Left"      -> pure RCLeft
    "Right"     -> pure RCRight
    "DownLeft"  -> pure RCDownLeft
    "Down"      -> pure RCDown
    "DownRight" -> pure RCDownRight
    _ -> fail "Unknown RelativeCell"

puzzleID :: Puzzle -> PuzzleID
puzzleID pzl =
    -- We should specificly give a canonical output for this datastructure,
    -- say alphabetical pretty printed key names, sorted lists (sorting lists depends on JSON data allowances).
    UUIDV5.generateNamed puzzleNS $ BSL.unpack $ JS.encode pzl
  where
    puzzleNS :: UUID
    puzzleNS = read "1238b96a-5c4f-40d5-a980-a3212d128af6"

blueprintID :: Blueprint -> BlueprintID
blueprintID bpnt =
    -- We should specificly give a canonical output for this datastructure,
    -- say alphabetical pretty printed key names, sorted lists (sorting lists depends on JSON data allowances).
    UUIDV5.generateNamed blueprintNS $ BSL.unpack $ JS.encode bpnt
  where
    blueprintNS :: UUID
    blueprintNS = read "e5f4ae68-da49-458c-969e-231c61d53a26"

data Folio
  = Folio
    { fPuzzle :: Puzzle
    , fBlueprint :: Blueprint
    , fSnapshot :: Maybe Snapshot
    }
  deriving (Show, Eq, Ord, NFData, Generic, Typeable)

instance JS.ToJSON Folio where
  toJSON (Folio p b ms) =
    JS.object ["puzzle" JS..= p, "blueprint" JS..= b, "snapshot" JS..= ms]
  toEncoding (Folio p b ms) =
    JS.pairs ("puzzle" JS..= p <> "blueprint" JS..= b <> "snapshot" JS..= ms)

instance JS.FromJSON Folio where
  parseJSON = JS.withObject "Folio" $ \o ->
    Folio
      <$> o JS..: "puzzle"
      <*> o JS..: "blueprint"
      <*> o JS..: "snapshot"

type Rate = Double

data GatewayBall
  = GatewayBall
    { gbType :: {-# UNPACK #-} !Int
    , gbRate :: {-# UNPACK #-} !Rate
    }
  deriving (Show, Eq, Ord, NFData, Generic, Typeable)

instance JS.ToJSON GatewayBall where
  toJSON (GatewayBall typ rt) =
    JS.object ["type" JS..= typ, "rate" JS..= rt]
  toEncoding (GatewayBall typ rt) =
    JS.pairs ("type" JS..= typ <> "rate" JS..= rt)

instance JS.FromJSON GatewayBall where
  parseJSON = JS.withObject "GatewayBall" $ \o ->
    GatewayBall
      <$> o JS..: "type"
      <*> o JS..: "rate"

data Gateway
  = Gateway
    { gPos :: {-# UNPACK #-} !Position
    , gBalls :: {-# UNPACK #-} !(V.Vector GatewayBall)
    }
  deriving (Show, Eq, Ord, NFData, Generic, Typeable)

instance JS.ToJSON Gateway where
  toJSON (Gateway (x,y) typ) =
    JS.object ["x" JS..= x, "y" JS..= y, "balls" JS..= typ]
  toEncoding (Gateway (x,y) typ) =
    JS.pairs ("x" JS..= x <> "y" JS..= y <> "balls" JS..= typ)

instance JS.FromJSON Gateway where
  parseJSON = JS.withObject "Gateway" $ \o ->
    Gateway
      <$> ((,) <$> o JS..: "x" <*> o JS..: "y")
      <*> o JS..: "balls"

data Puzzle
  = Puzzle
    { pReqTiles :: V.Vector RelativeCell -- ^ Which tiles of the machine need to be completed for this puzzle to be ready to be solved.
    , pInputs   :: V.Vector Gateway
    , pOutputs  :: V.Vector Gateway
    , pSpec   :: JS.Object
    }
  deriving (Show, Eq, Ord, NFData, Generic, Typeable)

instance JS.ToJSON Puzzle where
  toJSON (Puzzle rqtls is os pzl) =
    JS.object ["reqTiles" JS..= rqtls, "inputs" JS..= is, "outputs" JS..= os, "spec" JS..= pzl]
  toEncoding (Puzzle rqtls is os pzl) =
    JS.pairs ("reqTiles" JS..= rqtls <> "inputs" JS..= is <> "outputs" JS..= os <> "spec" JS..= pzl)

instance JS.FromJSON Puzzle where
  parseJSON = JS.withObject "Puzzle" $ \o ->
    Puzzle
      <$> o JS..: "reqTiles"
      <*> o JS..: "inputs"
      <*> o JS..: "outputs"
      <*> o JS..: "spec"

data TileSize
  = TileSize {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Show, Eq, Ord, NFData, Typeable, Generic)

instance JS.ToJSON TileSize where
  toJSON (TileSize x y) = JS.object ["x" JS..= x, "y" JS..= y]
  toEncoding (TileSize x y) = JS.pairs $ ("x" JS..= x)<>("y" JS..= y)

instance JS.FromJSON TileSize where
  parseJSON = JS.withObject "tile_size pos" $ \o ->
    TileSize
      <$> o JS..: "x"
      <*> o JS..: "y"

-- | The overall machine.
data MetaMachine tile
  = MetaMachine
    { mmGrid        :: {-# UNPACK #-} !(Array (X, Y) tile)
    , mmTileSize    :: {-# UNPACK #-} !TileSize
    , mmMillisecondPerBall :: {-# UNPACK #-} !Double
    , mmPrioPuzzles :: {-# UNPACK #-} !(V.Vector PuzzleID)
    }
  deriving (Show, Functor, Eq, Ord, Traversable, Foldable, NFData, Generic, Typeable)

data VersionedMachine tile
  = VersionedMachine
    { vmVersion :: {-# UNPACK #-} !MachineVersion -- ^ Incrimented version of which edit of the metamachine this is
    , vmMachine :: MetaMachine tile
    }
  deriving (Show, Functor, Eq, Ord, Traversable, Foldable, NFData, Generic, Typeable)

data ModData
  = ModData
    { mdBlueprint :: Maybe BlueprintID
    , mdPuzzleID  :: PuzzleID
    , mdToMod     :: Maybe Integer
    }
  deriving (Show, Eq, Ord, NFData, Generic, Typeable)

bpidsInMachine :: GameState -> [BlueprintID]
bpidsInMachine = rights . Array.elems .  mmGrid

instance JS.ToJSON ModData where
  toJSON (ModData mbpid pid tm) =
    JS.object $ ("puzzle" JS..= pid):catMaybes [fmap ("to_mod" JS..=) tm, fmap ("blueprint" JS..=) mbpid]
  toEncoding (ModData mbpid pid tm) =
    JS.pairs $ ("puzzle" JS..= pid) <> maybe mempty ("to_mod" JS..=) tm <> maybe mempty ("blueprint" JS..=) mbpid

instance JS.FromJSON ModData where
  parseJSON = JS.withObject "Puzzle" $ \o ->
    ModData
      <$> o JS..:? "blueprint"
      <*> o JS..:  "puzzle"
      <*> o JS..:?  "to_mod"

gridizeArray :: (Ix x, Ix y) => Array (x, y) a -> [[a]]
gridizeArray a =
  let ((lbx, lby), (hbx, hby)) = Array.bounds a
  in (`map` Ix.range (lby, hby)) $ \y  ->
        (`map` Ix.range (lbx, hbx)) $ \x ->
           a Array.! (x, y)

jsonizeMetaMachine :: JS.ToJSON tile => MetaMachine tile -> [JS.Pair]
jsonizeMetaMachine (MetaMachine g ts rt pp) =
    [ "tile_size" JS..= ts
    , "ms_per_ball" JS..= rt
    , "prio_puzzles" JS..= pp
    , "grid" JS..= gridizeArray g
    ]
{-# INLINE jsonizeMetaMachine #-}

jsonizeVersionedMachine :: JS.ToJSON tile => VersionedMachine tile -> [JS.Pair]
jsonizeVersionedMachine (VersionedMachine v mm) =
    ("version" JS..= v):jsonizeMetaMachine mm
{-# INLINE jsonizeVersionedMachine #-}

jsonEncodeMetaMachine :: JS.ToJSON tile => MetaMachine tile -> JS.Series
jsonEncodeMetaMachine (MetaMachine g ts rt pp) =
       ("tile_size" JS..= ts)
    <> ("ms_per_ball" JS..= rt)
    <> ("prio_puzzles" JS..= pp)
    <> ("grid" JS..= gridizeArray g)
{-# INLINE jsonEncodeMetaMachine #-}

jsonEncodeVersionedMachine :: JS.ToJSON tile => VersionedMachine tile -> JS.Series
jsonEncodeVersionedMachine (VersionedMachine v mm) =
    ("version" JS..= v) <> jsonEncodeMetaMachine mm
{-# INLINE jsonEncodeVersionedMachine #-}

minimalEitherPuzzleBlueprint :: Either PuzzleID BlueprintID -> JS.Value
minimalEitherPuzzleBlueprint = JS.object . (\case { Left pid -> ["puzzle" JS..= pid]; Right tid -> ["blueprint" JS..= tid]})

instance {-# OVERLAPPING  #-} JS.ToJSON GameState where
  toJSON = JS.object . jsonizeMetaMachine . fmap minimalEitherPuzzleBlueprint
  {-# INLINE toJSON #-}
  toEncoding = JS.pairs . jsonEncodeMetaMachine . fmap minimalEitherPuzzleBlueprint
  {-# INLINE toEncoding #-}

instance {-# OVERLAPPABLE #-} JS.ToJSON tile => JS.ToJSON (MetaMachine tile) where
  toJSON = JS.object . jsonizeMetaMachine
  {-# INLINE toJSON #-}
  toEncoding = JS.pairs . jsonEncodeMetaMachine
  {-# INLINE toEncoding #-}

instance {-# OVERLAPPING  #-} JS.ToJSON VersionedGameState where
  toJSON = JS.object . jsonizeVersionedMachine . fmap minimalEitherPuzzleBlueprint
  toEncoding = JS.pairs . jsonEncodeVersionedMachine . fmap minimalEitherPuzzleBlueprint
  {-# INLINE toEncoding #-}

instance {-# OVERLAPPABLE #-} JS.ToJSON tile => JS.ToJSON (VersionedMachine tile) where
  toJSON = JS.object . jsonizeVersionedMachine
  toEncoding = JS.pairs . jsonEncodeVersionedMachine
  {-# INLINE toEncoding #-}

parseIDObj :: JS.FromJSON a => JS.Key -> JS.Value -> JS.Parser a
parseIDObj k = JS.withObject (show k) $ \o -> o JS..: k

instance {-# OVERLAPPING  #-} JS.FromJSON GameState where
  parseJSON =
    JS.withObject "MetaMachine" $ \o -> do
      gridList <- (o JS..: "grid") >>=
                     JS.withArray "Grid-Y" (traverse $
                        JS.withArray "Grid-X" $ traverse $ \v ->
                          Left <$> parseIDObj "puzzle" v <|> Right <$> parseIDObj "blueprint" v)
      let bndy = length gridList
      guard (bndy > 0)
      let bndx = length $ V.head gridList
      guard (bndx > 0)
      let grid = concatMap (\(y, xl) -> zipWith (\ x v -> ((x, y), v)) [0..] xl) $ zip [0..] $ V.toList (V.toList <$> gridList)
      p <- o JS..: "tile_size"
      rt <- o JS..: "ms_per_ball"
      pp <- (o JS..: "prio_puzzles") <|> pure mempty
      pure $ MetaMachine (Array.array ((0,0), (bndx-1, bndy-1)) grid) p rt pp

instance {-# OVERLAPPABLE #-} JS.FromJSON tile => JS.FromJSON (MetaMachine tile) where
  parseJSON =
    JS.withObject "MetaMachine" $ \o -> do
      gridList <- o JS..: "grid"
      let bndy = length gridList
      let bndx = length $ head gridList
      let grid = concatMap (\(y, xl) -> zipWith (\ x v -> ((x, y), v)) [0..] xl) $ zip [0..] gridList
      p <- o JS..: "tile_size"
      rt <- o JS..: "ms_per_ball"
      pp <- (o JS..: "prio_puzzles") <|> pure mempty
      pure $ MetaMachine (Array.array ((0,0), (bndx-1, bndy-1)) grid) p rt pp

instance {-# OVERLAPPING  #-} JS.FromJSON VersionedGameState where
  parseJSON v =
    (\f -> JS.withObject "VersionedMachine" f v) $ \o -> do
      version <- o JS..: "version"
      machine <- JS.parseJSON v
      pure $ VersionedMachine version machine

instance {-# OVERLAPPABLE #-} JS.FromJSON tile => JS.FromJSON (VersionedMachine tile) where
  parseJSON v =
    (\f -> JS.withObject "VersionedMachine" f v) $ \o -> do
      version <- o JS..: "version"
      machine <- JS.parseJSON v
      pure $ VersionedMachine version machine

data Blueprint
  = Blueprint
    { bPuzzleID    :: {-# UNPACK #-} !PuzzleID -- ^ which puzzle this is supposed to solve.
    -- Title Block stuff
    , bTitle       :: {-# UNPACK #-} !Text
    , bSubmittedAt :: {-# UNPACK #-} !(Maybe UTCTime) -- Client can't send us the submitted date
    -- Actual submission
    , bWidgets     :: {-# UNPACK #-} !(HashMap Int JS.Object)
    }
  deriving (Show, Eq, Ord, NFData, Generic, Typeable)

data WidgetSignature
  = WidgetSignature
    { wName   :: {-# UNPACK #-} !Text
    , wX      :: {-# UNPACK #-} !Double
    , wY      :: {-# UNPACK #-} !Double
    , wAngle  :: {-# UNPACK #-} !(Maybe Double)
    , wWidth  :: {-# UNPACK #-} !(Maybe Double)
    , wHeight :: {-# UNPACK #-} !(Maybe Double)
    , wRadius :: {-# UNPACK #-} !(Maybe Double)
    }
  deriving (Show, Eq, Ord, NFData, Generic, Typeable)

instance S.Serial WidgetSignature

instance JS.FromJSON WidgetSignature where
  parseJSON = JS.withObject "WidgetSignature" $ \o -> WidgetSignature
        <$> o JS..: "type"
        <*> o JS..: "x"
        <*> o JS..: "y"
        <*> o JS..:? "angle"
        <*> o JS..:? "width"
        <*> o JS..:? "height"
        <*> o JS..:? "radius"

instance JS.ToJSON WidgetSignature where
  toJSON (WidgetSignature ty x y a w h r) = JS.object $ catMaybes [ Just ("type"  JS..= ty)
    , Just ("x"     JS..= x)
    , Just ("y"     JS..= y)
    , fmap ("angle" JS..= ) a
    , fmap ("width" JS..= ) w
    , fmap ("height" JS..= ) h
    , fmap ("radius" JS..= ) r
    ]
  toEncoding (WidgetSignature ty x y a w h r) = JS.pairs $ mconcat $ catMaybes [ Just ("type"  JS..= ty)
    , Just ("x"     JS..= x)
    , Just ("y"     JS..= y)
    , fmap ("angle" JS..= ) a
    , fmap ("width" JS..= ) w
    , fmap ("height" JS..= ) h
    , fmap ("radius" JS..= ) r
    ]

instance JS.FromJSON Blueprint where
  parseJSON = JS.withObject "Blueprint" $ \o -> Blueprint
        <$> o JS..: "puzzle"
        <*> o JS..: "title"
        <*> o JS..:? "submittedAt"
        <*> o JS..: "widgets"

instance JS.ToJSON Blueprint where
  toJSON (Blueprint p a t s) = JS.object
    [ "puzzle" JS..= p
    , "title" JS..= a
    , "submittedAt" JS..= t
    , "widgets" JS..= s
    ]
  toEncoding (Blueprint p a t s) = JS.pairs $
        ("puzzle" JS..= p)
     <> ("title" JS..= a)
     <> ("submittedAt" JS..= t)
     <> ("widgets" JS..= s)

data InspectionReport
  = InspectionReport
    { irBlueprint :: {-# UNPACK #-} !BlueprintID
    , irSnapshot :: {-# UNPACK #-} !Snapshot
    }
  deriving (Show, Eq, Ord, NFData, Generic, Typeable)

type Snapshot = JS.Object

instance JS.FromJSON InspectionReport where
  parseJSON = JS.withObject "InspectionReport" $ \o -> InspectionReport
        <$> o JS..: "blueprint"
        <*> o JS..: "snapshot"

instance JS.ToJSON InspectionReport where
  toJSON (InspectionReport bp ss) = JS.object
    [ "blueprint" JS..= bp
    , "snapshot" JS..= ss
    ]
  toEncoding (InspectionReport bp ss) = JS.pairs $
       ("blueprint" JS..= bp)
    <> ("snapshot" JS..= ss)

data MachineShop s
  = MachineShop
    { sdPuzzles :: {-# UNPACK #-} !(HashMap PuzzleID Puzzle)
    , sdMachineDesign :: {-# UNPACK #-} !(MetaMachine PuzzleID)
    , sdModLogins :: {-# UNPACK #-} !(HashMap UserName UserDigest)
    , sdStore :: !s
    , sdBaseUrl :: {-# UNPACK #-} !Text
    , sdOrigins :: {-# UNPACK #-} ![BS.ByteString]
    -- | Caches
    , sdMachineByVersion :: {-# UNPACK #-} !(AtomicLRU MachineVersion GameState)
    , sdReadyPuzzlesByVersion :: {-# UNPACK #-} !(AtomicLRU MachineVersion (HashMap PuzzleID Puzzle))
    , sdBlueprintCache :: {-# UNPACK #-} !(AtomicLRU BlueprintID Blueprint)
    , sdSnapshotCache :: {-# UNPACK #-} !(AtomicLRU BlueprintID Snapshot)
    , sdDeltaCache :: {-# UNPACK #-} !(AtomicLRU (MachineVersion, MachineVersion) (MachineUpdates BlueprintID))
    , sdPuzzleLocCache :: {-# UNPACK #-} !(HashMap PuzzleID (UV.Vector (X,Y)))
    , sdBlueprint2PuzzleCache :: {-# UNPACK #-} !(AtomicLRU BlueprintID PuzzleID)
    }

cachePuzzleLoc :: MetaMachine PuzzleID -> HashMap PuzzleID (UV.Vector (X, Y))
cachePuzzleLoc = fmap UV.fromList . HashMap.fromListWith (<>) . map (fmap pure . swap) . Array.assocs . mmGrid

{-
data ServerSharedState
   = ServerSharedState
     { sssForcedPuzzles :: {-# UNPACK #-} !(V.Vector PuzzleID)
     }
  deriving (Show, Eq, Generic, Typeable)

instance JS.ToJSON ServerSharedState
instance JS.FromJSON ServerSharedState

instance Semigroup ServerSharedState where
  (ServerSharedState fp0) <> (ServerSharedState fp1) = ServerSharedState (fp0<>fp1)

instance Monoid ServerSharedState where
  mempty = ServerSharedState mempty
-}

data MachineUpdates a
  = MachineUpdates
    { muTileSize   :: {-# UNPACK #-} !TileSize
    , muGridSize :: {-# UNPACK #-} !(X, Y)
    , muMillisecondPerBall :: {-# UNPACK #-} !Double
    , muPriorityPuzzles :: {-# UNPACK #-} !(V.Vector PuzzleID)
    , muConstruction :: {-# UNPACK #-} !(V.Vector ((X,Y), a))
    }
  deriving (Show, Eq, Ord, Functor, Traversable, Foldable, NFData, Generic, Typeable)

instance JS.FromJSON a => JS.FromJSON (MachineUpdates a) where
  parseJSON = JS.withObject "MachineUpdates" $ \o -> MachineUpdates
        <$> o JS..: "tile_size"
        <*> o JS..: "grid_size"
        <*> o JS..: "ms_per_ball"
        <*> o JS..: "prio_puzzle"
        <*> o JS..: "construction"

instance JS.ToJSON a => JS.ToJSON (MachineUpdates a) where
  toJSON (MachineUpdates ts gs r pp c) = JS.object
    [ "tile_size" JS..= ts
    , "grid_size" JS..= gs
    , "ms_per_ball" JS..= r
    , "prio_puzzle" JS..= pp
    , "construction" JS..= c
    ]
  toEncoding (MachineUpdates ts gs r pp c) = JS.pairs $
       ("tile_size" JS..= ts)
    <> ("grid_size" JS..= gs)
    <> ("ms_per_ball" JS..= r)
    <> ("prio_puzzle" JS..= pp)
    <> ("construction" JS..= c)

-- | Finds the thing to set in a first machine to generate the second machine.
deltaMachine :: Eq a => MetaMachine a -> MetaMachine a -> MachineUpdates a
deltaMachine (MetaMachine { mmGrid = g0 }) (MetaMachine { mmTileSize=ts, mmMillisecondPerBall=rt, mmPrioPuzzles=pp, mmGrid = g1 }) =
  MachineUpdates
    ts
    (snd $ Array.bounds g1) -- Safe because we require a (0,0) start of grid.
    rt
    pp $
    V.fromList $ mapMaybe (\(i, a1) ->
      let a0 = if Ix.inRange (Array.bounds g0) i then Just (g0 Array.! i) else Nothing
      in if a0==Just a1 then Nothing else Just (i, a1)
      ) $ Array.assocs g1

applyDelta :: MetaMachine a -> MachineUpdates a -> MetaMachine a
applyDelta oldMachine updates =
  MetaMachine newGrid (muTileSize updates) (muMillisecondPerBall updates) (muPriorityPuzzles updates)
  where
    newGrid =
      let oldAssocs = HashMap.fromList $ Array.assocs $ mmGrid oldMachine
          newAssocs = HashMap.fromList $ V.toList $ muConstruction updates
      in Array.array ((0,0), muGridSize updates) $ (`map` Ix.range ((0,0), muGridSize updates)) $ \gl ->
        maybe (error "incomplete information for applyDelta") (gl,)$ HashMap.lookup gl newAssocs <|> HashMap.lookup gl oldAssocs

translateMachine :: Monad m => ((X, Y) -> a -> m b) -> MetaMachine a -> m (MetaMachine b)
translateMachine act m = do
  g1 <- fmap (Array.array (Array.bounds (mmGrid m))) $ mapM (\(i, a) -> (i,) <$> act i a) $ Array.assocs $ mmGrid m
  pure (m { mmGrid = g1 })

firstPuzzles :: HashMap PuzzleID Puzzle -> GameState -> [(PuzzleID, Puzzle)]
firstPuzzles pzls m = mapMaybe (\case {(_, Right _) -> Nothing; (_, Left pid) -> (pid,) <$> HashMap.lookup pid pzls}) $ sort $ fmap (first snd) $ Array.assocs $ mmGrid m

findReadyPuzzles :: HashMap PuzzleID Puzzle -> GameState -> [(PuzzleID, Puzzle)]
findReadyPuzzles pzls m =
  mapMaybe puzzleReady $ Array.assocs $ mmGrid m
  where
    puzzleReady :: ((X, Y), Either PuzzleID BlueprintID) -> Maybe (PuzzleID, Puzzle)
    puzzleReady (_, Right _)  = Nothing -- If its a finished blueprint, it can't be a ready puzzle
    puzzleReady (p, Left pid) = do
      pzl <- HashMap.lookup pid pzls -- Uh, a non existant puzzle can't be ready, but we probably want to be loud about this?
      let rdy = all (maybe True isRight . getRelative (mmGrid m) p) $ pReqTiles pzl -- All required cells are Blueprints?
      if rdy then pure (pid, pzl) else Nothing

isSolved :: GameState -> Bool
isSolved = all isRight

isSolvable :: HashMap PuzzleID Puzzle -> GameState -> Bool
isSolvable pm gs | not (isSolved gs) && null (findReadyPuzzles pm gs) = False
isSolvable _ gs | isSolved gs = True
isSolvable pm gs = isSolvable pm setSolvedReady
  where
    rdySet = fst <$> findReadyPuzzles pm gs
    setSolvedReady = gs { mmGrid = (\case
                                            r@(Right _) -> r
                                            Left pzlID | pzlID `elem` rdySet -> Right (read "00000000-0000-0000-0000-000000000000")
                                            l@(Left _) -> l
                                        ) <$> mmGrid gs
                        }

remanufacture :: MetaMachine Puzzle -> MetaMachine (Either PuzzleID Blueprint) -> GameState
remanufacture machineDesign oldMachine =
  runIdentity $ translateMachine repairPart $
    machineDesign { mmPrioPuzzles = cleanPrio }
  where
    cleanPrio = V.filter (`HashSet.member` (HashSet.fromList $ fmap puzzleID $ Array.elems $ mmGrid machineDesign)) $ mmPrioPuzzles oldMachine
    repairPart :: (X, Y) -> Puzzle -> Identity (Either PuzzleID BlueprintID)
    -- If it is a puzzle, always the new puzzle because its the same or updated.
    repairPart xy pzl = do
      let pid = puzzleID pzl
      case if Ix.inRange (Array.bounds $ mmGrid oldMachine) xy then Just (mmGrid oldMachine Array.! xy) else Nothing of
        Nothing -> pure $ Left pid
        Just (Left _) -> pure $ Left pid
        Just (Right bp) | bPuzzleID bp /= pid -> pure $ Left pid
        Just (Right bp) -> pure $ Right $ blueprintID bp

--data VertShift = ...
--data HoriShift = ...

-- | Gets the value in a cell at a relative position to a given cell, or Nothing if that is outside the grid.
getRelative :: Array (X,Y) a -> (X, Y) -> RelativeCell -> Maybe a
getRelative g (x0, y0) r = do
    let rp = bimap (x0 +) (y0 +) $ relMap r
    if Ix.inRange (Array.bounds g) rp then Just (g Array.! rp) else Nothing
  where
    -- This is the only thing that orients the grid I guess ... orientation ...
    -- 4th quadrant grid.
    relMap :: RelativeCell -> (X, Y)
    relMap RCUpLeft    = (-1, -1)
    relMap RCUp        = ( 0, -1)
    relMap RCUpRight   = ( 1, -1)
    relMap RCLeft      = (-1,  0)
    relMap RCRight     = ( 1,  0)
    relMap RCDownLeft  = (-1,  1)
    relMap RCDown      = ( 0,  1)
    relMap RCDownRight = ( 1,  1)

-- TODO stub for bullshit nonsense
newtype BallTypes
   = BallType
     { _ballType :: Char
     }
   deriving Show
