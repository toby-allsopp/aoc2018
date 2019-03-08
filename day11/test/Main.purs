module Test.Main where

import Prelude

import Day11 as Day11
import Effect (Effect)
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  test "part 1" do
    Assert.equal 4 $ Day11.cellPower 8 { x: 3, y: 5 }
    Assert.equal (-5) $ Day11.cellPower 57 { x: 122, y: 79 }
    Assert.equal 0 $ Day11.cellPower 39 { x: 217, y: 196 }
    Assert.equal 4 $ Day11.cellPower 71 { x: 101, y: 153 }
    Assert.equal 29 $ Day11.squarePower 18 { x: 33, y: 45 }
    Assert.equal { power: 29, coords: { x: 33, y: 45 } } $ Day11.mostPowerfulSquare 18
    Assert.equal { power: 30, coords: { x: 21, y: 61 } } $ Day11.mostPowerfulSquare 42