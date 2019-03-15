module Main where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (fromJust)
import Day12 as Day12
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)

inputString :: String
inputString = """initial state: ###.#..#..##.##.###.#.....#.#.###.#.####....#.##..#.#.#..#....##..#.##...#.###.#.#..#..####.#.##.#

#.... => .
#.##. => #
..#.. => .
#.#.# => .
.#.## => #
...## => #
##... => #
###.. => #
#..## => .
.###. => .
###.# => #
..... => .
#..#. => .
.#.#. => #
##..# => #
.##.. => .
...#. => .
#.### => .
..### => .
####. => .
#.#.. => #
.##.# => #
.#... => #
##.#. => #
....# => .
..#.# => #
#...# => #
..##. => .
.#..# => #
.#### => .
##### => #
##.## => #
"""

exampleString :: String
exampleString = """initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #
"""

process :: String -> Number -> Effect Unit
process s n =
  case Day12.parseInput s of
    Left error -> log error
    Right { rules, initialState } -> do
      --log $ show input
      --void $ sequence $ log <$> show <$> Day12.generations input.rules n input.initialState
      let stable = Day12.iterateGenerationsUntilStableDiff rules initialState
      log $ show stable
      log $ show $ Day12.nthGeneration rules (stable.generation + 100) initialState # Day12.sumPlantNumbers
      log $ show $ stable.sum + stable.diff * 100
      if n < (Int.toNumber stable.generation)
        then
          log $ show $ Day12.nthGeneration rules (unsafePartial $ fromJust $ Int.fromNumber n) initialState # Day12.sumPlantNumbers
        else
          log $ show $ (Int.toNumber stable.sum) + (Int.toNumber stable.diff) * (n - (Int.toNumber stable.generation))

main :: Effect Unit
main = do
  process exampleString 20.0
  process inputString 20.0
  process inputString 50000000000.0