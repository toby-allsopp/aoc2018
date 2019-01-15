module Test.Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.HashMap as HashMap
import Data.Maybe (Maybe(..))
import Day4 (groupLinesIntoShifts, parseInput, sleepiestGuard, sleepiestMinutePerGuard, sleepsPerMinutePerGuard, sortLines, strategy1, sumSleepTimesPerGuard)
import Effect (Effect)
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

input :: String
input = """[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:25] wakes up
[1518-11-01 00:05] falls asleep
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-02 00:50] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-02 00:40] falls asleep
[1518-11-05 00:55] wakes up"""

main :: Effect Unit
main = runTest do
  test "strategy 1" do
    let lines = parseInput input
    Assert.equal (Right 17) $ lines <#> Array.length
    let shifts = (lines <#> sortLines) >>= groupLinesIntoShifts
    Assert.equal (Right 10) $ (shifts <#> sumSleepTimesPerGuard) >>= sleepiestGuard
    Assert.equal (Right $ Just $ Just 24) $ shifts <#> sleepsPerMinutePerGuard <#> sleepiestMinutePerGuard <#> HashMap.lookup 10
    Assert.equal (Right {minute: 24, guardId: 10}) $ shifts >>= strategy1
