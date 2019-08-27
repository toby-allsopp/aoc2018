module Main where

import Prelude

import Day22 (Depth(..), Pos(..))
import Day22 as Day22
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  let cave = Day22.erosionLevels (Depth 510) (Pos {x: 10, y: 10})
  log $ show cave
  let rtcave = Day22.erosionLevelsToRegionTypes cave
  log $ show rtcave
  log $ show (Day22.riskLevel rtcave)
  log $ show $ Day22.riskLevel $ Day22.erosionLevelsToRegionTypes $ Day22.erosionLevels (Depth 9171) (Pos {x: 7, y: 721})
