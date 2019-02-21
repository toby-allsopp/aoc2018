module Test.Main where

import Prelude

import Day8 as Day8
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

input :: String
input = """2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"""

main :: Effect Unit
main = runTest do
  let tree = input # Day8.parseInput
  test "part 1" do
    Assert.equal (Just 138) $ tree <#> Day8.sumMetadata
  -- test "part 2" do
  --   Assert.equal (Right { t : 15, steps : ['C', 'A', 'B', 'F', 'D', 'E'] }) $ coords <#> Day7.stepsInOrderWithWorkers 2 (fromEnum >>> (_ - fromEnum 'A') >>> (_ + 1))
