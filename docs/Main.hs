{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE InstanceSigs #-}
module Main where

import           Control.Lens
import qualified Data.Aeson as JS
import qualified Data.Array as Array
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import           Data.OpenApi (OpenApi, SecurityScheme (..), _openApiComponents, _componentsSecuritySchemes, SecurityDefinitions (..), allOperations, security, SecurityRequirement (SecurityRequirement))
import qualified Data.OpenApi as OpenApi
import           Data.Time (UTCTime)
import qualified Data.UUID as UUID
import Incredible.API
import Incredible.Data
import Options.Applicative
import Servant.OpenApi (HasOpenApi(toOpenApi))
import Data.Data
import qualified Data.Text as T
import Servant (BasicAuth)
import qualified Servant.API

newtype IncredibleDocsOptions = IncredibleDocsOptions {
  incredibleDocsOutputPath :: FilePath
}

incredibleOpts :: Parser IncredibleDocsOptions
incredibleOpts = do
  IncredibleDocsOptions <$> configPath
  where
    configPath = strOption $ mconcat [
                      long "output"
                    , short 'o'
                    , metavar "OUTPUT"
                    , help "Path to the incredible docs output"
                    ]

main :: IO ()
main = do
  opts <- execParser $ info (incredibleOpts <**> helper) fullDesc
  BSL.writeFile (incredibleDocsOutputPath opts) $ JS.encode incredibleSwagger

incredibleSwagger :: OpenApi
incredibleSwagger = toOpenApi incredibleAPI
  & OpenApi.info . OpenApi.title   .~ "Incredible API"
  & OpenApi.info . OpenApi.version .~ "0"
  & OpenApi.info . OpenApi.description ?~ "Swagger API docs for Incredible"

instance OpenApi.ToSchema Folio where
  declareNamedSchema _ = do
    puzzleScheme <- OpenApi.declareSchemaRef (Proxy :: Proxy Puzzle)
    blueprintScheme <- OpenApi.declareSchemaRef (Proxy :: Proxy Blueprint)
    msnapshotScheme <- OpenApi.declareSchemaRef (Proxy :: Proxy (Maybe Snapshot))
    pure $ OpenApi.NamedSchema (Just "Folio") $ mempty
      & OpenApi.type_         ?~ OpenApi.OpenApiObject
      & OpenApi.properties    .~ InsOrdHashMap.fromList
                                 [ ("puzzle", puzzleScheme)
                                 , ("blueprint", blueprintScheme)
                                 , ("snapshot", msnapshotScheme)
                                 ]
      & OpenApi.required      .~ ["puzzle", "blueprint", "snapshot"]

instance OpenApi.ToSchema TileSize where
  declareNamedSchema _ = do
    intScheme <- OpenApi.declareSchemaRef (Proxy :: Proxy Int)
    pure $ OpenApi.NamedSchema (Just "TileSize") $ mempty
      & OpenApi.type_         ?~ OpenApi.OpenApiObject
      & OpenApi.properties    .~ InsOrdHashMap.fromList
                                 [ ("x", intScheme)
                                 , ("y", intScheme)                                 ]
      & OpenApi.required      .~ ["x", "y"]

instance OpenApi.ToSchema (MetaMachine (Maybe BlueprintID)) where
  declareNamedSchema _ = do
    mBlueprintIDRef <- OpenApi.declareSchemaRef (Proxy::Proxy [[Maybe BlueprintID]])
    doubleSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy Double)
    tileSizeSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy TileSize)
    prioPzlSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy [PuzzleID])
    pure $ OpenApi.NamedSchema (Just "MetaMachine (Maybe BlueprintID)") $ mempty
      & OpenApi.type_         ?~ OpenApi.OpenApiObject
      & OpenApi.properties    .~ InsOrdHashMap.fromList
                                 [ ("grid", mBlueprintIDRef)
                                 , ("tile_size", tileSizeSchema)
                                 , ("ms_per_ball", doubleSchema)
                                 , ("prio_puzzle", prioPzlSchema)
                                 ]
      & OpenApi.required      .~ ["grid", "tile_size", "ms_per_ball"]
      & OpenApi.example ?~ JS.toJSON exampleMetaMachineMaybeBlueprintID

instance OpenApi.ToSchema (MetaMachine (Maybe Blueprint)) where
  declareNamedSchema _ = do
    mBlueprintRef <- OpenApi.declareSchemaRef (Proxy::Proxy [[Maybe Blueprint]])
    doubleSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy Double)
    tileSizeSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy TileSize)
    prioPzlSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy [PuzzleID])
    pure $ OpenApi.NamedSchema (Just "MetaMachine (Maybe Blueprint)") $ mempty
      & OpenApi.type_         ?~ OpenApi.OpenApiObject
      & OpenApi.properties    .~ InsOrdHashMap.fromList
                                 [ ("grid", mBlueprintRef)
                                 , ("tile_size", tileSizeSchema)
                                 , ("ms_per_ball", doubleSchema)
                                 , ("prio_puzzle", prioPzlSchema)
                                 ]
      & OpenApi.required      .~ ["grid", "tile_size", "ms_per_ball"]
      & OpenApi.example ?~ JS.toJSON exampleMetaMachineMaybeBlueprint

instance OpenApi.ToSchema (MetaMachine ModData) where
  declareNamedSchema _ = do
    modDataRef <- OpenApi.declareSchemaRef (Proxy::Proxy [[ModData]])
    doubleSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy Double)
    tileSizeSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy TileSize)
    prioPzlSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy [PuzzleID])
    pure $ OpenApi.NamedSchema (Just "MetaMachine ModData") $ mempty
      & OpenApi.type_         ?~ OpenApi.OpenApiObject
      & OpenApi.properties    .~ InsOrdHashMap.fromList
                                 [ ("grid", modDataRef)
                                 , ("tile_size", tileSizeSchema)
                                 , ("ms_per_ball", doubleSchema)
                                 , ("prio_puzzle", prioPzlSchema)
                                 ]
      & OpenApi.required      .~ ["grid", "tile_size", "ms_per_ball"]
      & OpenApi.example ?~ JS.toJSON exampleMetaMachineModData

instance OpenApi.ToSchema (VersionedMachine (Maybe BlueprintID)) where
  declareNamedSchema _ = do
    blueprintIDRef <- OpenApi.declareSchemaRef (Proxy::Proxy [[Maybe BlueprintID]])
    doubleSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy Double)
    tileSizeSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy TileSize)
    prioPzlSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy [PuzzleID])
    pure $ OpenApi.NamedSchema (Just "VersionedMachine (Maybe BlueprintID)") $ mempty
      & OpenApi.type_         ?~ OpenApi.OpenApiObject
      & OpenApi.properties    .~ InsOrdHashMap.fromList
                                 [ ("grid", blueprintIDRef)
                                 , ("version", OpenApi.toSchemaRef (Proxy :: Proxy Integer))
                                 , ("tile_size", tileSizeSchema)
                                 , ("ms_per_ball", doubleSchema)
                                 , ("prio_puzzle", prioPzlSchema)
                                 ]
      & OpenApi.required      .~ ["grid", "version", "tile_size", "ms_per_ball"]
      & OpenApi.example ?~ JS.toJSON exampleVersionedMachineMaybeBlueprintID

instance OpenApi.ToSchema (VersionedMachine (Maybe Blueprint)) where
  declareNamedSchema _ = do
    mBlueprintRef <- OpenApi.declareSchemaRef (Proxy::Proxy [[Maybe Blueprint]])
    doubleSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy Double)
    tileSizeSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy TileSize)
    prioPzlSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy [PuzzleID])
    pure $ OpenApi.NamedSchema (Just "VersionedMachine (Maybe Blueprint)") $ mempty
      & OpenApi.type_         ?~ OpenApi.OpenApiObject
      & OpenApi.properties    .~ InsOrdHashMap.fromList
                                 [ ("grid", mBlueprintRef)
                                 , ("version", OpenApi.toSchemaRef (Proxy :: Proxy Integer))
                                 , ("tile_size", tileSizeSchema)
                                 , ("ms_per_ball", doubleSchema)
                                 , ("prio_puzzle", prioPzlSchema)
                                 ]
      & OpenApi.required      .~ ["grid", "version", "tile_size", "ms_per_ball"]
      & OpenApi.example ?~ JS.toJSON exampleVersionedMachineMaybeBlueprint

instance OpenApi.ToSchema (VersionedMachine ModData) where
  declareNamedSchema _ = do
    modDataRef <- OpenApi.declareSchemaRef (Proxy::Proxy [[ModData]])
    doubleSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy Double)
    tileSizeSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy TileSize)
    prioPzlSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy [PuzzleID])
    pure $ OpenApi.NamedSchema (Just "VersionedMachine ModData") $ mempty
      & OpenApi.type_         ?~ OpenApi.OpenApiObject
      & OpenApi.properties    .~ InsOrdHashMap.fromList
                                 [ ("grid", modDataRef)
                                 , ("version", OpenApi.toSchemaRef (Proxy :: Proxy Integer))
                                 , ("tile_size", tileSizeSchema)
                                 , ("ms_per_ball", doubleSchema)
                                 , ("prio_puzzle", prioPzlSchema)
                                 ]
      & OpenApi.required      .~ ["grid", "version", "tile_size", "ms_per_ball"]
      & OpenApi.example ?~ JS.toJSON exampleVersionedMachineModData

instance OpenApi.ToSchema a => OpenApi.ToSchema (MachineUpdates a) where
  declareNamedSchema _ = do
    dataRef <- OpenApi.declareSchemaRef (Proxy::Proxy [((X, Y), a)])
    gridSizeRef <- OpenApi.declareSchemaRef (Proxy::Proxy ((X, Y), (X, Y)))
    doubleSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy Double)
    tileSizeSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy TileSize)
    prioPzlSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy [PuzzleID])
    pure $ OpenApi.NamedSchema (Just "VersionedMachine ModData") $ mempty
      & OpenApi.type_         ?~ OpenApi.OpenApiObject
      & OpenApi.properties    .~ InsOrdHashMap.fromList
                                 [ ("tile_size", tileSizeSchema)
                                 , ("grid_size", gridSizeRef)
                                 , ("ms_per_ball", doubleSchema)
                                 , ("construction", dataRef)
                                 , ("prio_puzzle", prioPzlSchema)
                                 ]
      & OpenApi.required      .~ ["tile_size", "ms_per_ball", "construction", "grid_size", "prio_puzzle"]

instance OpenApi.ToSchema RelativeCell where
  declareNamedSchema _ = do
    pure $ OpenApi.NamedSchema (Just "RelativeCell") $
      mempty
        & OpenApi.type_         ?~ OpenApi.OpenApiString
        & OpenApi.enum_         ?~ (map JS.toJSON $ enumFrom (minBound::RelativeCell))

instance OpenApi.ToSchema Puzzle where
  declareNamedSchema _ = do
    reqRef <- OpenApi.declareSchemaRef (Proxy::Proxy [RelativeCell])
    gatesRef <- OpenApi.declareSchemaRef (Proxy::Proxy [Gateway])
    objRef <- OpenApi.declareSchemaRef (Proxy::Proxy JS.Object)
    pure $ OpenApi.NamedSchema (Just "Puzzle") $
      mempty
        & OpenApi.type_         ?~ OpenApi.OpenApiObject
        & OpenApi.properties    .~ InsOrdHashMap.fromList
                                   [ ("reqTiles", reqRef)
                                   , ("inputs", gatesRef)
                                   , ("outputs", gatesRef)
                                   , ("spec", objRef)
                                   ]
        & OpenApi.required      .~ ["reqTiles", "inputs", "outputs", "spec"]
        & OpenApi.example ?~ JS.toJSON examplePuzzle

instance OpenApi.ToSchema Blueprint where
  declareNamedSchema _ = do
    utcRef <- OpenApi.declareSchemaRef (Proxy::Proxy (Maybe UTCTime))
    objRef <- OpenApi.declareSchemaRef (Proxy::Proxy JS.Object)
    pure $ OpenApi.NamedSchema (Just "Blueprint") $
      mempty
        & OpenApi.type_         ?~ OpenApi.OpenApiObject
        & OpenApi.properties    .~ InsOrdHashMap.fromList
                                   [ ("puzzle", OpenApi.Inline (mempty & OpenApi.type_ ?~ OpenApi.OpenApiString))
                                   , ("title", OpenApi.Inline (mempty & OpenApi.type_ ?~ OpenApi.OpenApiString))
                                   , ("submittedAt", utcRef)
                                   , ("widgets", objRef)
                                   ]
        & OpenApi.required      .~ ["puzzle", "title", "widgets"]
        & OpenApi.example ?~ JS.toJSON exampleBlueprint

instance OpenApi.ToSchema InspectionReport where
  declareNamedSchema _ = do
    bpRef <- OpenApi.declareSchemaRef (Proxy::Proxy BlueprintID)
    snapshotRef <- OpenApi.declareSchemaRef (Proxy::Proxy Snapshot)
    pure $ OpenApi.NamedSchema (Just "InspectionReport") $
      mempty
        & OpenApi.type_         ?~ OpenApi.OpenApiObject
        & OpenApi.properties    .~ InsOrdHashMap.fromList
                                   [ ("blueprint", bpRef)
                                   , ("snapshot", snapshotRef)
                                   ]
        & OpenApi.required      .~ ["blueprint", "snapshot"]

instance OpenApi.ToSchema ModData where
  declareNamedSchema _ =
    pure $ OpenApi.NamedSchema (Just "ModData") $
      mempty
        & OpenApi.type_         ?~ OpenApi.OpenApiObject
        & OpenApi.properties    .~ InsOrdHashMap.fromList
                                   [ ("puzzle", OpenApi.Inline $ mempty & OpenApi.type_ ?~ OpenApi.OpenApiString)
                                   , ("blueprint", OpenApi.Inline $ mempty & OpenApi.type_ ?~ OpenApi.OpenApiString)
                                   , ("to_mod", OpenApi.Inline $ mempty & OpenApi.type_ ?~ OpenApi.OpenApiInteger)
                                   ]
        & OpenApi.required      .~ ["puzzle"]
        & OpenApi.example ?~ JS.toJSON exampleModData

instance OpenApi.ToSchema GatewayBall where
  declareNamedSchema _ = do
    doubleSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy Double)
    intRef <- OpenApi.declareSchemaRef (Proxy :: Proxy Int)
    pure $ OpenApi.NamedSchema (Just "GatewayBall") $
      mempty
        & OpenApi.type_         ?~ OpenApi.OpenApiObject
        & OpenApi.properties    .~ InsOrdHashMap.fromList
                                   [ ("type", intRef)
                                   , ("rate", doubleSchema)
                                   ]
        & OpenApi.required      .~ ["type", "rate"]

instance OpenApi.ToSchema Gateway where
  declareNamedSchema _ = do
    doubleSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy Double)
    gwBallsRef <- OpenApi.declareSchemaRef (Proxy :: Proxy [GatewayBall])
    pure $ OpenApi.NamedSchema (Just "Gateway") $
      mempty
        & OpenApi.type_         ?~ OpenApi.OpenApiObject
        & OpenApi.properties    .~ InsOrdHashMap.fromList
                                   [ ("x", doubleSchema)
                                   , ("y", doubleSchema)
                                   , ("balls", gwBallsRef)
                                   ]
        & OpenApi.required      .~ ["x", "y", "balls"]
        & OpenApi.example ?~ JS.toJSON exampleModData

instance HasOpenApi api => HasOpenApi (BasicAuth realm auth Servant.API.:> api) where
  toOpenApi Proxy = addSecurity $ toOpenApi $ Proxy @api
   where
    addSecurity = addSecurityRequirement identifier . addSecurityScheme identifier securityScheme
    identifier :: T.Text = "BasicAuth"
    securityScheme =
      SecurityScheme
        { _securitySchemeType = OpenApi.SecuritySchemeHttp OpenApi.HttpSchemeBasic
        , _securitySchemeDescription = Just "Basic Authentication"
        }

addSecurityScheme :: T.Text -> SecurityScheme -> OpenApi -> OpenApi
addSecurityScheme securityIdentifier securityScheme openApi =
  openApi
    { _openApiComponents =
        (_openApiComponents openApi)
          { _componentsSecuritySchemes =
              _componentsSecuritySchemes (_openApiComponents openApi)
                <> SecurityDefinitions (InsOrdHashMap.singleton securityIdentifier securityScheme)
          }
    }

addSecurityRequirement :: T.Text -> OpenApi -> OpenApi
addSecurityRequirement securityRequirement =
  allOperations
    . security
    %~ ((SecurityRequirement $ InsOrdHashMap.singleton securityRequirement []) :)

exampleBlueprint :: Blueprint
exampleBlueprint =
  Blueprint (read "00000000-0000-0000-0000-000000000000"::UUID.UUID)
            "Lauren Ipsum"
            Nothing
            mempty

examplePuzzle :: Puzzle
examplePuzzle =
  Puzzle [RCUpLeft] [Gateway (0.5, 0) [GatewayBall 1 1]] [Gateway (0.5, 1) [GatewayBall 1 1]] mempty

exampleModData :: ModData
exampleModData = ModData (Just (read "00000000-0000-0000-0000-000000000000"::UUID.UUID)) (read "00000000-0000-0000-0000-000000000000"::UUID.UUID) (Just 20)

exampleVersionedMachineMaybeBlueprintID :: VersionedMachine (Maybe BlueprintID)
exampleVersionedMachineMaybeBlueprintID = VersionedMachine 0 exampleMetaMachineMaybeBlueprintID

exampleMetaMachineMaybeBlueprintID :: MetaMachine (Maybe BlueprintID)
exampleMetaMachineMaybeBlueprintID =
  MetaMachine (Array.array ((0,0),(1,1))
                           [ ((0,0), Just (read "00000000-0000-0000-0000-000000000000"::UUID.UUID))
                           , ((0,1), Just (read "00000000-0000-0000-0000-000000000000"::UUID.UUID))
                           , ((1,0), Just (read "00000000-0000-0000-0000-000000000000"::UUID.UUID))
                           , ((1,1), Nothing)
                           ]) (TileSize 700 700) 0.001 mempty

exampleVersionedMachineMaybeBlueprint :: VersionedMachine (Maybe Blueprint)
exampleVersionedMachineMaybeBlueprint = VersionedMachine 0 exampleMetaMachineMaybeBlueprint

exampleMetaMachineMaybeBlueprint :: MetaMachine (Maybe Blueprint)
exampleMetaMachineMaybeBlueprint =
  MetaMachine (Array.array ((0,0),(1,1))
                           [ ((0,0), Just exampleBlueprint)
                           , ((0,1), Nothing)
                           , ((1,0), Just exampleBlueprint)
                           , ((1,1), Nothing)
                           ]) (TileSize 700 700) 0.001 mempty

exampleVersionedMachineModData :: VersionedMachine ModData
exampleVersionedMachineModData = VersionedMachine 0 exampleMetaMachineModData

exampleMetaMachineModData :: MetaMachine ModData
exampleMetaMachineModData =
  MetaMachine (Array.array ((0,0),(1,1))
                           [ ((0,0), ModData (Just (read "00000000-0000-0000-0000-000000000000"::UUID.UUID)) (read "00000000-0000-0000-0000-000000000000"::UUID.UUID) Nothing)
                           , ((0,1), ModData Nothing (read "00000000-0000-0000-0000-000000000000"::UUID.UUID) $ Just 11)
                           , ((1,0), ModData Nothing (read "00000000-0000-0000-0000-000000000000"::UUID.UUID) $ Just 20)
                           , ((1,1), ModData Nothing (read "00000000-0000-0000-0000-000000000000"::UUID.UUID) Nothing)
                           ]) (TileSize 700 700) 0.001 mempty
