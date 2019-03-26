module Day14 where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial, unsafeCrashWith)

type State = {
    scoreboard :: Array Int,
    elfPositions :: Array Int
}

type MutState r = {
    scoreboard :: STArray r Int,
    elfPositions :: STArray r Int
}

unsafeFreezeState :: forall r. MutState r -> ST r State
unsafeFreezeState mut = do
    scoreboard <- STArray.unsafeFreeze mut.scoreboard
    elfPositions <- STArray.unsafeFreeze mut.elfPositions
    pure { scoreboard, elfPositions }

thawState :: forall r. State -> ST r (MutState r)
thawState state = do
    scoreboard <- STArray.thaw state.scoreboard
    elfPositions <- STArray.thaw state.elfPositions
    pure { scoreboard, elfPositions }

fromJustWithReason :: forall a. Partial => (Unit -> String) -> Maybe a -> a
fromJustWithReason reason Nothing = unsafeCrashWith (reason unit)
fromJustWithReason _ (Just x) = x

unsafeScoreAt :: Partial => Int -> State -> Int
unsafeScoreAt index state =
    fromJustWithReason (\_ -> "invalid index " <> show index <> " for state " <> show state)
    $ Array.index state.scoreboard index

sumElfScores :: State -> Int
sumElfScores { scoreboard, elfPositions } =
    unsafePartial
    $ sum
    $ fromJustWithReason (\_-> "invalid index")
    <$> Array.index scoreboard
    <$> elfPositions

digits :: Int -> Array Int
digits 0 = [0]
digits n = digits' [] n
    where
    digits' :: Array Int -> Int -> Array Int
    digits' a 0 = a
    digits' a n = digits' (Array.cons (n `mod` 10) a) (n `div` 10)

addRecipes :: forall r. Int -> MutState r -> ST r Unit
addRecipes sum mut = do
    void $ STArray.pushAll (digits sum) mut.scoreboard

updateElfPositions :: forall r. MutState r -> ST r Unit
updateElfPositions mut = do
    state <- unsafeFreezeState mut
    let numElfs = Array.length state.elfPositions
    ST.for 0 numElfs \i -> do
        elfPos <- STArray.peek i mut.elfPositions
        case unsafePartial $ updateElfPosition state.scoreboard <$> elfPos of
            Just pos -> STArray.poke i pos mut.elfPositions
            Nothing -> unsafeCrashWith "hmm"

updateElfPosition :: Partial => Array Int -> Int -> Int
updateElfPosition scoreboard current =
    (current + 1 + (Array.unsafeIndex scoreboard current)) `mod` (Array.length scoreboard)

turn :: forall r. MutState r -> ST r Unit
turn mut = do
    state <- unsafeFreezeState mut
    addRecipes (sumElfScores state) mut
    updateElfPositions mut

runUntil :: (State -> Boolean) -> State -> State
runUntil stop initialState = ST.run (thawState initialState >>= go)
    where
    go :: forall r. MutState r -> ST r State
    go mut = tailRecM go' unit
        where
        go' :: Unit -> ST r (Step Unit State)
        go' _ = do
            state <- unsafeFreezeState mut
            if stop state
                then pure (Done state)
                else turn mut *> pure (Loop unit)

atLeastRecipes :: Int -> State -> Boolean
atLeastRecipes n state = Array.length state.scoreboard >= n

recipesFrom :: Int -> Int -> State -> Array Int
recipesFrom start count state =
    runUntil (atLeastRecipes (start + count)) state
    # _.scoreboard
    # Array.drop start
    # Array.take count

subArrayEqual :: forall a. Eq a => Array a -> Int -> Int -> Array a -> Int -> Int -> Boolean
subArrayEqual a1 s1 e1 a2 s2 e2 =
    if s1 >= e1 then
        s2 >= e2
    else
        s2 < e2 && Array.index a1 s1 == Array.index a2 s2 && subArrayEqual a1 (s1 + 1) e1 a2 (s2 + 1) e2

lastMatches :: forall a. Eq a => Array a -> Array a -> Boolean
lastMatches a1 a2 = subArrayEqual a1 0 (Array.length a1) a2 (Array.length a2 - Array.length a1) (Array.length a2)

lastButOneMatches :: forall a. Eq a => Array a -> Array a -> Boolean
lastButOneMatches a1 a2 = subArrayEqual a1 0 (Array.length a1) a2 (Array.length a2 - Array.length a1 - 1) (Array.length a2 - 1)

lastOrLastButOneMatches :: Array Int -> State -> Boolean
lastOrLastButOneMatches recipes state =
    lastMatches recipes state.scoreboard
    || lastButOneMatches recipes state.scoreboard

countRecipesBefore :: Array Int -> State -> Int
countRecipesBefore recipes state =
    runUntil (lastOrLastButOneMatches (Array.fromFoldable recipes)) state
    # \{ scoreboard } -> if lastMatches (Array.fromFoldable recipes) scoreboard then (Array.length scoreboard) - (Array.length recipes) else (Array.length scoreboard) - (Array.length recipes) - 1
