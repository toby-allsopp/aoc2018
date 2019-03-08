module Main where

import Prelude

import Day11 as Day11
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main =
  log $ show $ Day11.mostPowerfulSquare 7403
