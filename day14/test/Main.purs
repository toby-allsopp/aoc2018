module Test.Main where

import Day14
import Prelude

import Control.Monad.ST as ST
import Data.Foldable (foldMap)
import Effect (Effect)
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  test "digits" do
    Assert.equal [0] $ digits 0
    Assert.equal [1] $ digits 1
    Assert.equal [1,0] $ digits 10
  let initialState = { scoreboard: [3, 7], elfPositions: [0, 1] }
  test "part 1" do
    let step state = ST.run do
          mut <- thawState state
          turn mut
          unsafeFreezeState mut
    Assert.equal { scoreboard: [3, 7, 1, 0], elfPositions: [0, 1] } $ step initialState
    Assert.equal { scoreboard: [3, 7, 1, 0, 1, 0], elfPositions: [4, 3] } $ (step >>> step) initialState
    Assert.equal { scoreboard: [3, 7, 1, 0, 1, 0, 1], elfPositions: [6, 4] } $ (step >>> step >>> step) initialState
    Assert.equal { scoreboard: [3, 7, 1, 0, 1, 0, 1, 2, 4, 5, 1, 5, 8, 9, 1, 6, 7, 7, 9], elfPositions: [6, 2]} $ runUntil (atLeastRecipes (9 + 10)) initialState
    Assert.equal "5158916779" $ foldMap show $ recipesFrom 9 10 initialState
    Assert.equal "0124515891" $ foldMap show $ recipesFrom 5 10 initialState
    Assert.equal "9251071085" $ foldMap show $ recipesFrom 18 10 initialState
    Assert.equal "5941429882" $ foldMap show $ recipesFrom 2018 10 initialState
  test "part 2" do
    Assert.equal 9 $ countRecipesBefore [5,1,5,8,9] initialState
    Assert.equal 5 $ countRecipesBefore [0,1,2,4,5] initialState
    Assert.equal 18 $ countRecipesBefore [9,2,5,1,0] initialState
    Assert.equal 2018 $ countRecipesBefore [5,9,4,1,4] initialState
