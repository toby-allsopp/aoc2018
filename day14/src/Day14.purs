module Day14 where

import Prelude

import Data.Array as Array
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial, unsafeCrashWith)

type State = {
    scoreboard :: Array Int,
    elfPositions :: Array Int
}

fromJustWithReason :: forall a. Partial => (Unit -> String) -> Maybe a -> a
fromJustWithReason reason Nothing = unsafeCrashWith (reason unit)
fromJustWithReason _ (Just x) = x

unsafeScoreAt :: Partial => Int -> State -> Int
unsafeScoreAt index state =
    fromJustWithReason (\_ -> "invalid index " <> show index <> " for state " <> show state)
    $ Array.index state.scoreboard index

sumElfScores :: State -> Int
sumElfScores { scoreboard, elfPositions } = unsafePartial $ sum $ Array.unsafeIndex scoreboard <$> elfPositions

digits :: Int -> Array Int
digits 0 = [0]
digits n = Array.reverse $ digits' [] n
    where
    digits' :: Array Int -> Int -> Array Int
    digits' a 0 = a
    digits' a n = digits' (Array.snoc a (n `mod` 10)) (n `div` 10)

addRecipes :: Int -> State -> State
addRecipes sum { scoreboard, elfPositions } = { scoreboard: scoreboard <> digits sum, elfPositions }

updateElfPositions :: State -> State
updateElfPositions state = state { elfPositions = unsafePartial $ updateElfPosition state <$> state.elfPositions }

updateElfPosition :: Partial => State -> Int -> Int
updateElfPosition state current =
    (current + 1 + (unsafeScoreAt current state)) `mod` (Array.length state.scoreboard)

turn :: State -> State
turn state = sumElfScores state # flip addRecipes state # updateElfPositions

runUntil :: (State -> Boolean) -> State -> State
runUntil stop state = if stop state then state else runUntil stop (turn state)

atLeastRecipes :: Int -> State -> Boolean
atLeastRecipes n state = Array.length state.scoreboard >= n

recipesFrom :: Int -> Int -> State -> Array Int
recipesFrom start count state =
    runUntil (atLeastRecipes (start + count)) state
    # _.scoreboard
    # Array.drop start
    # Array.take count