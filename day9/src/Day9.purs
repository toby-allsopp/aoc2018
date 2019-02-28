module Day9 (
    State,
    game,
    winningScore
) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe, fromJust, fromMaybe)
import Data.Foldable (class Foldable, foldl, foldr, foldMap, maximum)
import Data.Traversable (scanl)
import Partial.Unsafe (unsafePartial)

newtype CircularArray a = CircularArray (Array a)

caNormalizeIndex :: forall a. Array a -> Int -> Int
caNormalizeIndex a i = let result = i `mod` (Array.length a) in
    result

caSingleton :: forall a. a -> CircularArray a
caSingleton = CircularArray <<< Array.singleton

caReplicate :: forall a. Int -> a -> CircularArray a
caReplicate n = CircularArray <<< Array.replicate n

caIndex :: forall a. CircularArray a -> Int -> a
caIndex (CircularArray a) i = unsafePartial (Array.unsafeIndex a (caNormalizeIndex a i))

caInsertAt :: forall a. Int -> a -> CircularArray a -> CircularArray a
caInsertAt i x (CircularArray a) = CircularArray $ unsafePartial $ fromJust $ Array.insertAt (caNormalizeIndex a i) x a

caDeleteAt :: forall a. Int -> CircularArray a -> CircularArray a
caDeleteAt i (CircularArray a) = CircularArray $ unsafePartial $ fromJust $ Array.deleteAt (caNormalizeIndex a i) a

caModifyAt :: forall a. Int -> (a -> a) -> CircularArray a -> CircularArray a
caModifyAt i f (CircularArray a) = CircularArray $ unsafePartial $ fromJust $ Array.modifyAt (caNormalizeIndex a i) f a

derive instance eqCircularArray :: Eq a => Eq (CircularArray a)

instance showCircularArray :: Show a => Show (CircularArray a) where
    show (CircularArray a) = show a

instance circularArrayFoldable :: Foldable CircularArray where
    foldl f x (CircularArray a) = foldl f x a
    foldr f x (CircularArray a) = foldr f x a
    foldMap f (CircularArray a) = foldMap f a

data State = State {
    marbles :: CircularArray Int,
    currentIndex :: Int,
    scores :: CircularArray Int
}

derive instance eqState :: Eq State

instance showState :: Show State where
    show (State s) = show s

initialState :: Int -> State
initialState players = State {
    marbles: caSingleton 0,
    currentIndex: 0,
    scores: caReplicate players 0
}

placeMarble :: Int -> Int -> State -> State
placeMarble turn marbleScore (State { marbles: marbles@(CircularArray oldMarbles), currentIndex, scores }) =
    if marbleScore `mod` 23 == 0 then
        State {
            marbles: caDeleteAt (currentIndex - 7) marbles,
            currentIndex: caNormalizeIndex oldMarbles (currentIndex - 7),
            scores: caModifyAt turn (_ + (caIndex marbles (currentIndex - 7)) + marbleScore) scores
        }
    else
        let newMarbles@(CircularArray a) = caInsertAt (currentIndex + 2) marbleScore marbles in
        State {
            marbles: newMarbles,
            currentIndex: caNormalizeIndex oldMarbles (currentIndex + 2),
            scores
        }

takeTurn :: State -> Int -> State
takeTurn state turn = placeMarble turn (turn + 1) state

game :: Int -> Int -> Array State
game players lastMarbleScore =
    scanl takeTurn (initialState players) (Array.range 0 (lastMarbleScore - 1))

play :: Int -> Int -> State
play players turns = --game players turns # Array.last # fromMaybe (initialState players)
    foldl takeTurn (initialState players) (Array.range 0 (turns - 1))

winningScore :: Int -> Int -> Maybe Int
winningScore players lastMarbleScore =
    let State finalState = play players lastMarbleScore in
    finalState.scores # maximum