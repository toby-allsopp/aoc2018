module Test.Main where

import Array2d
import Day15
import Position
import Prelude

import Control.Monad.ST as ST
import Data.Array as Array
import Data.Either (Either(..), fromRight)
import Data.Foldable (foldMap, minimumBy)
import Data.HashSet as HashSet
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (trim)
import Data.String.Yarn (lines, unlines)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import ShortestPaths (labelToPath)
import ShortestPaths as SP
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

a2 :: forall a. Array (Array a) -> Array2d a
a2 rows = unsafePartial $ fromRight $ rowsToArray2d rows

m :: String -> { map :: Map, units :: Units }
m s = unsafePartial $ fromRight $ parseMap $ unlines $ trim <$> lines s

l :: Int -> Array Position -> Maybe SP.Label
l distance next = Just { distance, next: HashSet.fromFoldable next }

p :: Int -> Int -> Position
p = makePosition

u :: UnitType -> Position -> Int -> UnitState
u unitType position hitPoints = UnitState { unitType, position, attackPower: 3, hitPoints }

unitsToPositions :: Units -> Array Position
unitsToPositions units = unitsToArray units <#> unitPosition

main :: Effect Unit
main = runTest do
  test "path" do
    Assert.equal (Nothing) $ shortestPathsTo (a2 []) (Units []) (p 0 0) (p 0 0)
    Assert.equal (l 2 [p 1 0, p 0 1]) $
      shortestPathsTo (m "..\n..").map (Units []) (p 0 0) (p 1 1)
    Assert.equal (Nothing) $
      shortestPathsTo (m ".#\n..").map (Units []) (p 1 0) (p 0 0)
    Assert.equal [[p 0 0]] $
      unsafePartial $ SP.followPaths (a2 [[l 0 [], Nothing],[Nothing, Nothing]]) (p 0 0)
    Assert.equal [[p 1 0, p 0 0]] $
      unsafePartial $ SP.followPaths (a2 [[l 0 [], l 1 [p 0 0]],[Nothing, Nothing]]) (p 1 0)
    let sps = shortestPathsTo (m """....
                                    #...
                                    ###.
                                    ....""").map (Units []) (p 1 1)
    -- Assert.equal (a2 [[l 2 [p 1 0], l 1 [p 1 1], l 2 [p 1 0, p 2 1], l 3 [p 2 0, p 3 1]],
    --                   [Nothing,     l 0 [],      l 1 [p 1 1],        l 2 [p 2 1]],
    --                   [Nothing,     Nothing,     Nothing,            l 3 [p 3 1]],
    --                   [l 7 [p 1 3], l 6 [p 2 3], l 5 [p 3 3],        l 4 [p 3 2]]]) sps
    Assert.equal (l 7 [p 2 1]) $
      sps (p 0 3)
    Assert.equal (l 3 [p 1 0, p 2 1]) $
      sps (p 3 0)
    Assert.equal (Just (p 1 0)) $ nextStep <<< _.nexts =<< labelToPath <$> sps (p 3 0)
  test "move" do
    let { map, units } = m """#######
#E..G.#
#...#.#
#.G.#G#
#######"""
    Assert.equal (Units [makeUnit Elf (p 1 1), makeUnit Goblin (p 4 1), makeUnit Goblin (p 2 3), makeUnit Goblin (p 5 3)]) units
    let u1 = makeUnit Elf (p 1 1)
    -- Assert.equal (Just $ []) --Tuple (p 3 1) { distance: 2, nexts: [p 2 1] }) $
    --     let unit = u1 in
    --     let targets = targetsOfUnit units unit in
    --     if Array.null targets then
    --         Nothing
    --     else
    --         let inRange = inRangeOfTarget unit map units targets in
    --         let sps = labelShortestPathsTo map (removeUnitAt (unitPosition unit) units) <$> inRange in
    --         let spsFrom = SP.shortestPathsFrom (unitPosition unit) <$> sps in
    --         let pathsToNearestInRange = minimumBy compareDistanceThenReadingOrder $ Array.mapMaybe sequenceSnd $ Array.zip inRange $ (SP.shortestPathsFrom (unitPosition unit) <$> sps) in
    --         Just spsFrom
    --         -- let movedUnit = unsafePartial $ pathsToNearestInRange <#> (snd >>> _.nexts) >>= nextStep <#> updateUnitPosition unit # fromJust in --fromMaybe unit in
    --         -- Just movedUnit

    Assert.equal (Just (makeUnit Elf (p 2 1))) $ move map u1 units
    Assert.equal
      (Just (makeUnit Goblin (p 4 2)))
      ((let { map, units } = m """#########
                                  #G..G..G#
                                  #.......#
                                  #.......#
                                  #G..E..G#
                                  #.......#
                                  #.......#
                                  #G..G..G#
                                  #########"""
            unit = makeUnit Goblin (p 4 1) in
        move map unit units))
    Assert.equal
      (Just ((m """#########
             #G.....G#
             #...G...#
             #.......#
             #G..E..G#
             #.......#
             #.......#
             #G..G..G#
             #########""").units # unitsToPositions))
      ((let { map, units } = m """#########
                                  #G..G..G#
                                  #.......#
                                  #.......#
                                  #G..E..G#
                                  #.......#
                                  #.......#
                                  #G..G..G#
                                  #########"""
            unit = makeUnit Goblin (p 4 1) in
        let movedUnit = move map unit units in
        (\u -> updateUnitInUnits unit u units) <$> movedUnit) <#> unitsToPositions)
    Assert.equal
      (Just ((m """#########
             #.G....G#
             #...G...#
             #.......#
             #G..E..G#
             #.......#
             #.......#
             #G..G..G#
             #########""").units # unitsToPositions))
      ((let { map, units } = m """#########
                                  #G.....G#
                                  #...G...#
                                  #.......#
                                  #G..E..G#
                                  #.......#
                                  #.......#
                                  #G..G..G#
                                  #########"""
            unit = makeUnit Goblin (p 1 1) in
        let movedUnit = move map unit units in
        (\u -> updateUnitInUnits unit u units) <$> movedUnit) <#> unitsToPositions)
    Assert.equal
      (Left (Units [makeUnit Elf (p 0 0)]))
      (let { map, units } = m """E""" in round map units)
    Assert.equal
      (pure ((m """#########
                   #.G...G.#
                   #...G...#
                   #...E..G#
                   #.G.....#
                   #.......#
                   #G..G..G#
                   #.......#
                   #########""").units # unitsToPositions))
      ((let { map, units } = m """#########
                                  #G..G..G#
                                  #.......#
                                  #.......#
                                  #G..E..G#
                                  #.......#
                                  #.......#
                                  #G..G..G#
                                  #########""" in round map units) <#> unitsToPositions)
    Assert.equal
      (Right ((m """#########
#..G.G..#
#...G...#
#.G.E.G.#
#.......#
#G..G..G#
#.......#
#.......#
#########""").units # unitsToPositions))
      ((let { map, units } = m """#########
             #.G...G.#
             #...G...#
             #...E..G#
             #.G.....#
             #.......#
             #G..G..G#
             #.......#
             #########""" in round map units) <#> unitsToPositions)
    Assert.equal
      (Right ((m """#########
#.......#
#..GGG..#
#..GEG..#
#G..G...#
#......G#
#.......#
#.......#
#########""").units # unitsToPositions))
      ((let { map, units } = m """#########
#..G.G..#
#...G...#
#.G.E.G.#
#.......#
#G..G..G#
#.......#
#.......#
#########""" in round map units) <#> unitsToPositions)
  test "attack" do
    let { map, units } = m """#######
                              #.G...#
                              #...EG#
                              #.#.#G#
                              #..G#E#
                              #.....#
                              #######"""
    Assert.equal
      (Right [u Goblin (p 3 1) 200,
              u Elf (p 4 2) 197, u Goblin (p 5 2) 197,
              u Goblin (p 3 3) 200, u Goblin (p 5 3) 197,
              u Elf (p 5 4) 197])
      ((round map units) <#> unitsToArray)
    Assert.equal
      (Right [u Goblin (p 3 1) 200,
              u Elf (p 4 2) 197, u Goblin (p 5 2) 197,
              u Goblin (p 3 3) 200, u Goblin (p 5 3) 197,
              u Elf (p 5 4) 197])
      ((rounds 1 map units) <#> unitsToArray)
    Assert.equal
      (Right [u Goblin (p 4 1) 200,
              u Goblin (p 3 2) 200, u Elf (p 4 2) 188, u Goblin (p 5 2) 194,
              u Goblin (p 5 3) 194,
              u Elf (p 5 4) 194])
      ((rounds 2 map units) <#> unitsToArray)
    Assert.equal
      (Right [u Goblin (p 4 1) 200,
              u Goblin (p 3 2) 200, u Goblin (p 5 2) 131,
              u Goblin (p 5 3) 131,
              u Elf (p 5 4) 131])
      ((rounds 23 map units) <#> unitsToArray)
    Assert.equal
      (Right [u Goblin (p 1 1) 200,
              u Goblin (p 2 2) 131,
              u Goblin (p 5 3) 59,
              u Goblin (p 5 5) 200])
      ((rounds 47 map units) <#> unitsToArray)
    Assert.equal
      (Left $ Units [u Goblin (p 1 1) 200,
                     u Goblin (p 2 2) 131,
                     u Goblin (p 5 3) 59,
                     u Goblin (p 5 5) 200])
      (rounds 48 map units)
    Assert.equal
      ({ rounds: 47,
         units: Units [u Goblin (p 1 1) 200,
                       u Goblin (p 2 2) 131,
                       u Goblin (p 5 3) 59,
                       u Goblin (p 5 5) 200] })
      (battle map units)
    Assert.equal 27730 $ outcome $ battle map units
  test "examples" do
    let testBattle expected input =
          let { map, units } = m input in Assert.equal expected $ outcome $ battle map units
    testBattle 36334 """#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######"""
    testBattle 39514 """#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######"""
    testBattle 27755 """#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
#######"""
    testBattle 28944 """#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######"""
    testBattle 18740 """#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########"""
