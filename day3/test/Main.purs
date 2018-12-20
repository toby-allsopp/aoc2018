module Test.Main where

import Prelude (Unit, discard, ($), (<#>))
import Day3 (claimRectangles, countOverclaimedElements, parseInput, uncontestedClaims)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

testInput :: String
testInput = """#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2"""

main :: Effect Unit
main = runTest do
  test "part1" do
    Assert.equal (Just 4) $ parseInput testInput <#> claimRectangles <#> countOverclaimedElements
  test "part2" do
    Assert.equal (Just [3]) $ parseInput testInput <#> uncontestedClaims
