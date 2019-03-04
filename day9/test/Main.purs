module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Day9 as Day9
import Effect (Effect)
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  test "part 1" do
    --Assert.equal [] $ Day9.game 9 25
    Assert.equal (Just 32) $ Day9.winningScore 9 25
    Assert.equal (Just 8317) $ Day9.winningScore 10 1618
    Assert.equal (Just 146373) $ Day9.winningScore 13 7999
    Assert.equal (Just 2764) $ Day9.winningScore 17 1104
    Assert.equal (Just 54718) $ Day9.winningScore 21 6111
    Assert.equal (Just 37305) $ Day9.winningScore 30 5807
