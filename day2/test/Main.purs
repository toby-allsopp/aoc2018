module Test.Main where

import Prelude

import Day2 as Day2
import Effect (Effect)
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  test "checksum" do
    Assert.equal 12 $ Day2.checksum ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]
  test "correct ids" do
    Assert.equal ["fgij"] $ Day2.lettersInCommonInCorrectBoxIds ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]
