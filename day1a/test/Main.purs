module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Day1 (firstRepeatedFrequencyChange, sumFrequencyChanges)
import Effect (Effect)
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  test "sum" do
    Assert.equal (Just 3) $ sumFrequencyChanges "+1\n+1\n+1"
    Assert.equal (Just 0) $ sumFrequencyChanges "+1\n+1\n-2"
    Assert.equal (Just (-6)) $ sumFrequencyChanges "-1\n-2\n-3\n"
  test "repeat" do
    Assert.equal (Just 0) $ firstRepeatedFrequencyChange "+1\n-1"
    Assert.equal (Just 10) $ firstRepeatedFrequencyChange "+3\n+3\n+4\n-2\n-4"
    Assert.equal (Just 5) $ firstRepeatedFrequencyChange "-6\n+3\n+8\n+5\n-6"
    Assert.equal (Just 14) $ firstRepeatedFrequencyChange "+7\n+7\n-2\n-7\n-4"
