module Test.Main where

import Prelude

import Day11
import Effect (Effect)
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  test "part 1" do
    Assert.equal 4 $ cellPower 8 (Coords { x: 3, y: 5 })
    Assert.equal (-5) $ cellPower 57 (Coords { x: 122, y: 79 })
    Assert.equal 0 $ cellPower 39 (Coords { x: 217, y: 196 })
    Assert.equal 4 $ cellPower 71 (Coords { x: 101, y: 153 })
    Assert.equal 29 $ squarePower (Square { coords: Coords { x: 33, y: 45 }, size: 3 }) (allSquarePowers 18)
    Assert.equal { power: 29, coords: Coords { x: 33, y: 45 }, size: 3 } $ mostPowerfulSquare 18
    Assert.equal { power: 30, coords: Coords { x: 21, y: 61 }, size: 3 } $ mostPowerfulSquare 42
  test "part 2" do
    Assert.equal { power: 113, coords: Coords { x: 90, y: 269 }, size: 16 } $ mostPowerfulAnySquare 18
    Assert.equal { power: 119, coords: Coords { x: 232, y: 251 }, size: 12 } $ mostPowerfulAnySquare 42
