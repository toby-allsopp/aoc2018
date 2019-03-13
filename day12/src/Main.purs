module Main where

import Prelude

import Day12 as Day12
import Data.Either (Either(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Console (log)

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

process :: String -> Effect Unit
process s =
  case Day12.parseInput s of
    Left error -> log error
    Right input -> do
      log $ show input
      void $ sequence $ log <$> show <$> Day12.generations input.rules 20 input.initialState
      log $ show $ Day12.nthGeneration input.rules 20 input.initialState # Day12.sumPlantNumbers

main :: Effect Unit
main = do
  process exampleString
  process inputString