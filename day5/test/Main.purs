module Test.Main where

import Prelude

import Day5 as Day5
import Effect (Effect)
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

input :: String
input = "dabAcCaCBAcCcaDA"

main :: Effect Unit
main = runTest do
  test "part 1" do
    Assert.equal [] $ "aA" # Day5.parseInput # Day5.react
    Assert.equal [] $ "aBbA" # Day5.parseInput # Day5.react
    Assert.equal (Day5.parseInput "abAB") $ "abAB" # Day5.parseInput # Day5.react
    Assert.equal (Day5.parseInput "aabAAB") $ "aabAAB" # Day5.parseInput # Day5.react
    Assert.equal (Day5.parseInput "dabCBAcaDA") $ input # Day5.parseInput # Day5.react
  test "part 2" do
    Assert.equal (Day5.parseInput "daDA") $ "dabAcCaCBAcCcaDA" # Day5.parseInput # Day5.reactRemovingOne   
