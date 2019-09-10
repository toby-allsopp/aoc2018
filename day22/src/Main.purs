module Main where

import Prelude

import Day22 (Depth(..), Pos(..))
import Day22 as Day22
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  let targetPos = Pos {x: 10, y: 10}
  let cave = Day22.erosionLevels (Depth 510) targetPos
  -- log $ show cave
  let rtcave = Day22.erosionLevelsToRegionTypes cave
  -- log $ show rtcave
  log $ show (Day22.riskLevel targetPos rtcave)
  let realTargetPos = Pos {x: 7, y: 721}
  let realrtcave = Day22.erosionLevelsToRegionTypes $ Day22.erosionLevels (Depth 9171) realTargetPos
  log "got realrtcave"
  log $ show $ Day22.riskLevel realTargetPos realrtcave
  log $ show $ Day22.foo rtcave targetPos
  log $ show $ Day22.foo realrtcave realTargetPos
