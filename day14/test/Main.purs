module Test.Main where

import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Day14
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Prelude
import Test.QuickCheck (Result(..), (<?>))
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)

main :: Effect Unit
main = runTest do
  test "part 1" do
    let initialState = { scoreboard: [3, 7], elfPositions: [0, 1] }
    Assert.equal { scoreboard: [3, 7, 1, 0], elfPositions: [0, 1] } $ turn initialState
    Assert.equal { scoreboard: [3, 7, 1, 0, 1, 0], elfPositions: [4, 3] } $ (turn >>> turn) initialState
    Assert.equal { scoreboard: [3, 7, 1, 0, 1, 0, 1], elfPositions: [6, 4] } $ (turn >>> turn >>> turn) initialState
    Assert.equal { scoreboard: [3, 7, 1, 0, 1, 0, 1, 2, 4, 5, 1, 5, 8, 9, 1, 6, 7, 7, 9], elfPositions: [6, 2]} $ runUntil (atLeastRecipes (9 + 10)) initialState
    Assert.equal "5158916779" $ foldMap show $ recipesFrom 9 10 initialState
    Assert.equal "0124515891" $ foldMap show $ recipesFrom 5 10 initialState
    Assert.equal "9251071085" $ foldMap show $ recipesFrom 18 10 initialState
    Assert.equal "5941429882" $ foldMap show $ recipesFrom 2018 10 initialState
