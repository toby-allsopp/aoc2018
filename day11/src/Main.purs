module Main where

import Prelude

import Day11 as Day11
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log $ show $ Day11.mostPowerfulSquare 7403
  log $ show $ Day11.mostPowerfulAnySquare 7403
