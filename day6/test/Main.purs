module Test.Main where

import Prelude

import Day6 as Day6
import Effect (Effect)
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

input :: String
input = """1, 1
1, 6
8, 3
3, 4
5, 5
8, 9"""

main :: Effect Unit
main = runTest do
  test "part 1" do
    Assert.equal (Right $ Just 17) $ input # Day6.parseInput <#> (Day6.findClosestCoords >>> Day6.largestFiniteArea)
--  test "part 2" do
