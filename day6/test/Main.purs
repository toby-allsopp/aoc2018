module Test.Main where

import Prelude

import Day6 as Day6
import Effect (Effect)
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.String.CodeUnits as String
import Data.Char as Char

input :: String
input = """1, 1
1, 6
8, 3
3, 4
5, 5
8, 9"""

renderCoordIndex :: Int -> Int -> String
renderCoordIndex distance i = String.singleton $ fromMaybe '?' $ Char.fromCharCode $ (Char.toCharCode (if distance == 0 then 'A' else 'a')) + i

renderClosestCoords :: Day6.Array2D Day6.Cell -> String
renderClosestCoords = Day6.array2dFoldi f ""
  where
    f s x y {closestCoordIndex, closestCoordDistance} = s <> (if x == 0 then "\n" else "") <> maybe "." (renderCoordIndex closestCoordDistance) closestCoordIndex

main :: Effect Unit
main = runTest do
  let coords = input # Day6.parseInput
  test "part 1" do
    Assert.equal (Right """
Aaaa.ccc
aaddeccc
adddeccC
.dDdeecc
b.deEeec
Bb.eeee.
bb.eeeff
bb.eefff
bb.ffffF""") $ coords <#> Day6.findClosestCoords <#> renderClosestCoords
    Assert.equal (Right $ Just {area: 17, coordIndex: 4}) $ coords <#> (Day6.findClosestCoords >>> Day6.largestFiniteArea)
  test "part 2" do
    Assert.equal (Right 16) $ coords <#> (Day6.sumDistancesToEachCoord >>> Day6.areaWithDistanceLessThan 32)
