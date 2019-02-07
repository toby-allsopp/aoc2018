module Day5 where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Char.Unicode (toLower)
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))

type PolymerUnit = Char

type UnitType = Char

type Polymer = Array PolymerUnit

isValidUnit :: Char -> Boolean
isValidUnit c = c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z'

unitType :: PolymerUnit -> UnitType
unitType = toLower

parseInput :: String -> Polymer
parseInput = toCharArray >>> Array.filter isValidUnit

doUnitsReact :: PolymerUnit -> PolymerUnit -> Boolean
doUnitsReact u1 u2 = u1 /= u2 && unitType u1 == unitType u2

react :: Polymer -> Polymer
react xs = ST.run (STArray.withArray (go 0) xs)
    where
        go :: forall h. Int -> STArray h PolymerUnit -> ST h Unit
        go i accum = do
            mu1 <- STArray.peek i accum
            mu2 <- STArray.peek (i+1) accum
            case Tuple mu1 mu2 of
                Tuple (Just u1) (Just u2) ->
                    if doUnitsReact u1 u2 then
                        do
                            _ <- STArray.splice i 2 [] accum
                            go (max 0 (i-1)) accum
                    else go (i+1) accum
                _ -> pure unit

unitTypesInPolymer :: Polymer -> Array UnitType
unitTypesInPolymer = Array.foldl (\types unit -> Set.insert (unitType unit) types) Set.empty >>> Set.toUnfoldable

removeUnitType :: UnitType -> Polymer -> Polymer
removeUnitType t = Array.filter (unitType >>> (_ /= t))

minBy :: forall f a b. Foldable f => Ord b => (a -> b) -> f a -> Maybe a
minBy f = foldl go Nothing
    where go Nothing x = Just x
          go (Just min) x = Just $ if (f x) < (f min) then x else min

reactRemovingOne :: Polymer -> Polymer
reactRemovingOne p =
    unitTypesInPolymer p
    <#> flip removeUnitType p
    <#> react
    # minBy Array.length
    # fromMaybe []