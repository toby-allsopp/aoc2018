module Main where

import Prelude

import Day14 as Day14
import Effect (Effect)
import Effect.Console (log)
import Data.Foldable (foldMap)

main :: Effect Unit
main = do
  let initialState = { scoreboard: [3, 7], elfPositions: [0, 1] }
  log $ foldMap show $ Day14.recipesFrom 77201 10 initialState