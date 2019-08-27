module Day22 where

import Prelude

import Control.Monad.ST (ST, for)
import Control.Monad.ST as ST
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromJust)
import Matrix (Matrix(..))
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import STMatrix (STMatrix(..))
import STMatrix as STMatrix

type Cave a = Matrix a

newtype GeologicIndex = GeologicIndex Int
newtype ErosionLevel = ErosionLevel Int

derive newtype instance showErosionLevel :: Show ErosionLevel

newtype Depth = Depth Int

newtype Pos = Pos { x :: Int, y :: Int }

erosionLevels :: Depth -> Pos -> Cave ErosionLevel
erosionLevels depth (Pos targetPos) = ST.run do
    cave <- STMatrix.new (targetPos.x + 1) (targetPos.y + 1) (ErosionLevel 0)
    for 0 (targetPos.x + 1) \x -> do
        for 0 (targetPos.y + 1) \y -> do
            gi <- unsafePartial $ erosionLevel depth x y cave
            STMatrix.set x y gi cave
    STMatrix.freeze cave

erosionLevel :: forall r. Partial => Depth -> Int -> Int -> STMatrix r ErosionLevel -> ST r ErosionLevel
erosionLevel depth x y cave =
    case x, y of
        0, 0 -> pure $ geologicIndexToErosionLevel depth $ GeologicIndex 0
        x', y' | x' == STMatrix.width cave - 1 && y' == STMatrix.height cave - 1 -> pure $ geologicIndexToErosionLevel depth $ GeologicIndex 0
        _, 0 -> pure $ geologicIndexToErosionLevel depth $ GeologicIndex $ x * 16807
        0, _ -> pure $ geologicIndexToErosionLevel depth $ GeologicIndex $ y * 48271
        _, _ -> do
            (ErosionLevel el1) <- fromJust <$> STMatrix.get (x - 1) y cave
            (ErosionLevel el2) <- fromJust <$> STMatrix.get x (y - 1) cave
            pure $ geologicIndexToErosionLevel depth $ GeologicIndex $ el1 * el2

geologicIndexToErosionLevel :: Depth -> GeologicIndex -> ErosionLevel
geologicIndexToErosionLevel (Depth depth) (GeologicIndex gi) = ErosionLevel $ (gi + depth) `mod` 20183

data RegionType = Rocky | Wet | Narrow

instance showRegionType :: Show RegionType where
    show Rocky = "."
    show Wet = "="
    show Narrow = "|"

erosionLevelsToRegionTypes :: Cave ErosionLevel -> Cave RegionType
erosionLevelsToRegionTypes = map erosionLevelToRegionType

erosionLevelToRegionType :: ErosionLevel -> RegionType
erosionLevelToRegionType (ErosionLevel el) = case el `mod` 3 of
    0 -> Rocky
    1 -> Wet
    2 -> Narrow
    x -> unsafeCrashWith (show x)

riskLevel :: Cave RegionType -> Int
riskLevel = map regionTypeToRiskLevel >>> sum

regionTypeToRiskLevel :: RegionType -> Int
regionTypeToRiskLevel Rocky = 0
regionTypeToRiskLevel Wet = 1
regionTypeToRiskLevel Narrow = 2