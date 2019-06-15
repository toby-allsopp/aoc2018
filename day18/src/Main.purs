module Main where

import Prelude

import Data.Array (all)
import Data.Array as Array
import Data.Either (Either(..))
import Data.HashMap as HashMap
import Data.List.Lazy (drop, dropWhile, head, iterate, scanrLazy, slice, take, zip, (!!))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Day18 as Day18
import Effect (Effect)
import Effect.Console (log, logShow)

testinput :: String
testinput = """.#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|."""

input :: String
input = """..##|#.##..|#...|.#|....#|.#......|#......#|....|.
..#|.#.#|..#.|...|.|.|....|..|||||..#|..#.#..|##.|
.||.....#..#.....|||#|.....#|###|||.|..#...#..|##.
#|...|#|.......##.|......####.|..||#....##||.#...#
..|.##|#.|.#||#....#||...|#.||.|....|.|#|.#...#.#.
#..|......#..#....|||.||..#..#..#.|.|.|#.||.....#.
|.|...#|..|#|.|....#...#.|.#||.....#........||..|.
.#|##.|...|......|.#||#|#..|.|....|....|||...#####
.|.......#....##|.#.#...|.||.....#|.|#.......|##|.
.#....#|##|..##|..#.|...##.|#.##..#.......||.|.|##
###..#||........#...#..#..|......||.......#.|#|#..
.||.#.....|.#...|......#.||##||......|...||.||....
..#|.|....#.#.|||#...#.....#.#.#.|....#.|...|#....
#...|..#.|.|...#|..#.|#...|.......#.|.......|.###.
.|#|.#.|..#|....|..|..#..#|......#..#..|.#...|.|#.
.#...#......#|||..|.|.....#....|#.||.....#||##..|.
|.|...#||..|........#.....|#....|...||..##.#.#.|..
.....|......######|...|.....##.........|#|.#|.....
|..|.......|#|.##.|..|....#....##..||..|...|..|...
.||||#....|..|.|#|..|...#.|#.|.....|.||.||#...|...
.#|#..###.#|....#..||...|##..#.#|..#..|||........#
..|.#.....#|..|.#..|...#||......##.|....|.|#.|.|||
..#.......|#||..|...|.....##..#.#.####..|......#|#
.|##......|#....|..|.||...##|#....##||#.#|#.#..#.|
#..#..|..#....|..|....##..|..#....##.#|#|##|#|....
|####..#....|..|..|....|#.|....|.....##.##.#|....|
..||...##..|...#|##..|.##......#...##.|....#.|...#
.#...|#.|#|.....|#|....##.|.........|.......|.#...
||...###...|#..###|..|.#.|#||...#...#|.....|##|..|
#.#.#|....#|#..|..........|#..#|.|#||...|##.##.|#.
....|.##..#...#..##|..|....|..||#.|..|..#..#......
.|.#..|.#...||..#..|.|...#....|.||#.|#.....||.|...
..#|||.#..|#|...||#.|....|.#...#||||#...#...|...|#
..#..#....#|.............##...|..#..#..||##|..#.||
#....#|...#..##....###..||..#||...|.#..|.....|....
....|..#...#...||..||....|#|#|.|..|.#.|..|.##..|#.
..#.....|....||.##..#..#|..|.|#.....|...|..|..#..#
.##.||.#||..#|.#....||.|.....#|.....#....||..#.##|
..|.#|.|...|........#......|.##.|#.#..|......##...
.##||.|.##....|...##.#.....#.##.##..#...|||#|#.|.|
....|||..|....#..#.#..|.|.|....#.|#.#.##|.|#.#|.#.
..|...#|#....##.#|##.#.||##...#.|#..##.....#...#..
.|#..#.....|...|.#..##......|..#.|.......#.....#..
.#..|.#..|#...#....|..||.|..#..#...##........#....
.|.##.#|.#.#.|..||##|..||||.##|||..#..##...|..#|#.
#.......#...|#.|#||..|.##...#...|....|...##....#.|
.###..|......||#...|..||||#....|.||...#....|.#...|
.|.#...|#..|.....#......|.......|.........|.#.#...
|.|...#...|#|||...|||....|#..|#...#.#..#...|....#|
|#...#..#.|#|.#..#.#.....|.|.##...#.|#..|.#|..#..."""

main :: Effect Unit
main = case Day18.parseScan input of
  Left err -> log err
  Right pscan -> do
    -- log "parsed"
    -- logShow pscan
    let allMinutes = iterate Day18.changedScan pscan
    let after10 = allMinutes !! 10
    -- logShow after10
    logShow $ Day18.resourceValue <$> after10
    let allValues = allMinutes <#> Day18.resourceValue
    -- logShow $ Array.fromFoldable $ take 20 allMinutes
    -- logShow $ Array.fromFoldable $ take 100 allValues
    let allValuesWithMinute = zip allMinutes (iterate (_+1) 0)
    -- logShow $ Array.fromFoldable $ take 100 allValuesWithMinute
    let allValueToMinutes = scanrLazy (\(Tuple k v) -> HashMap.insertWith (<>) k [v]) HashMap.empty allValuesWithMinute
    -- logShow $ Array.fromFoldable $ take 10 allValueToMinutes
    let valueToMinutesAfter1000 = allValueToMinutes !! 1000
    logShow $ HashMap.values <$> valueToMinutesAfter1000
    let fromFirstDuplicate = dropWhile (HashMap.values >>> all (Array.length >>> (_ == 1))) allValueToMinutes
    case head fromFirstDuplicate of
      Nothing -> log "WTF"
      Just firstDuplicate -> do
        case Array.filter (\(Tuple rv minutes) -> Array.length minutes > 1) $ HashMap.toArrayBy Tuple $ firstDuplicate of
          [Tuple _ [firstMinute, secondMinute]] -> do
            logShow $ [firstMinute, secondMinute]
            logShow $ Array.fromFoldable $ take 100 $ drop firstMinute allValues
            let repeatedPart = slice firstMinute secondMinute allValues
            logShow $ Array.fromFoldable repeatedPart
            logShow $ allValues !! 1000
            logShow $ repeatedPart !! (1000 - firstMinute) `mod` (secondMinute - firstMinute)
            logShow $ repeatedPart !! (1000000000 - firstMinute) `mod` (secondMinute - firstMinute)
          _ -> log "WTF2"
