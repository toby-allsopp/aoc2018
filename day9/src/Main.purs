module Main where

import Prelude

import Day9 as Day9
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log $ show $ Day9.winningScore 491 71058
  --log $ show $ input # Day8.parseInput <#> Day8.nodeValue
