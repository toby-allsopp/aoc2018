module Test.Main where

import Prelude

import Data.Either (Either(..))
import Day8 as Day8
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
    Assert.equal (Right "Tree { children: [Tree { children: [], metadata: [10,11,12] },Tree { children: [Tree { children: [], metadata: [99] }], metadata: [2] }], metadata: [1,1,2] }") $ show <$> tree
    Assert.equal (Right 138) $ tree <#> Day8.sumMetadata
  test "part 2" do
    Assert.equal (Right 66) $ tree <#> Day8.nodeValue
