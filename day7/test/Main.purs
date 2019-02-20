module Test.Main where

import Prelude

import Day7 as Day7
import Effect (Effect)
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Data.Either (Either(..))
import Data.Enum (fromEnum)

input :: String
input = """Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin."""

main :: Effect Unit
main = runTest do
  let coords = input # Day7.parseInput
  test "part 1" do
    Assert.equal (Right { t : 6, steps : ['C', 'A', 'B', 'D', 'F', 'E'] }) $ coords <#> Day7.stepsInOrderWithWorkers 1 (const 1)
  test "part 2" do
    Assert.equal (Right { t : 15, steps : ['C', 'A', 'B', 'F', 'D', 'E'] }) $ coords <#> Day7.stepsInOrderWithWorkers 2 (fromEnum >>> (_ - fromEnum 'A') >>> (_ + 1))
