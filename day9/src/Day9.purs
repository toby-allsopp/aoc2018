module Day9 (
    State,
    --game,
    winningScore
) where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Internal (STRef)
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.Foldable (class Foldable, foldl, foldr, foldMap, maximum)
import Data.Long (Long)
import Data.Long as Long
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

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

type CLData r a = { data :: a, prev :: STRef r (CircularList r a), next :: STRef r (CircularList r a) }
data CircularList r a = CircularList (CLData r a) | Empty

clUnsafeFromNonEmpty :: forall r a. CircularList r a -> CLData r a
clUnsafeFromNonEmpty (CircularList l) = l
clUnsafeFromNonEmpty Empty = unsafeCrashWith "unexpected empty CircularList"

clUnsafePrev :: forall r a. CircularList r a -> ST r (CLData r a)
clUnsafePrev = clUnsafeFromNonEmpty >>> (_.prev) >>> STRef.read >=> (pure <<< clUnsafeFromNonEmpty)

clUnsafeNext :: forall r a. CircularList r a -> ST r (CLData r a)
clUnsafeNext = clUnsafeFromNonEmpty >>> (_.next) >>> STRef.read >=> (pure <<< clUnsafeFromNonEmpty)

clSingleton :: forall r a. a -> ST r (CircularList r a)
clSingleton x = do
    p <- STRef.new Empty
    n <- STRef.new Empty
    let mcl = CircularList { data: x, prev: p, next: n }
    _ <- STRef.write mcl p
    STRef.write mcl n

clAdvance :: forall r a. Int -> CircularList r a -> ST r (CircularList r a)
clAdvance 0 l = pure l
clAdvance n (CircularList l) | n > 0 = do
    next <- STRef.read l.next
    clAdvance (n - 1) next
clAdvance n (CircularList l) | otherwise = do
    prev <- STRef.read l.prev
    clAdvance (n + 1) prev
clAdvance _ Empty = pure Empty

clHead :: forall r a. CircularList r a -> Maybe a
clHead (CircularList l) = Just l.data
clHead Empty = Nothing

clInsertHead :: forall r a. a -> CircularList r a -> ST r (CircularList r a)
clInsertHead x n@(CircularList ndata) = do
    -- insert l between p and n
    -- p = n.prev
    -- l.prev = p
    -- l.next = n
    -- p.next = l
    -- n.prev = l
    p <- STRef.read ndata.prev
    let pdata = clUnsafeFromNonEmpty p
    next <- STRef.new n
    prev <- STRef.new p
    let l = CircularList { data: x, prev, next }
    _ <- STRef.write l pdata.next
    _ <- STRef.write l ndata.prev
    pure l
clInsertHead x Empty = clSingleton x

clDeleteHead :: forall r a. CircularList r a -> ST r (CircularList r a)
clDeleteHead (CircularList ldata) = do
    p <- STRef.read ldata.prev
    let pdata = clUnsafeFromNonEmpty p
    n <- STRef.read ldata.next
    let ndata = clUnsafeFromNonEmpty n
    _ <- STRef.write p ndata.prev
    STRef.write n pdata.next
clDeleteHead Empty = pure Empty

data State r = State {
    marbles :: CircularList r Int,
    scores :: CircularArray Long
}

-- derive instance eqState :: Eq State

-- instance showState :: Show (State r) where
--     show (State s) = show s

initialState :: forall r. Int -> ST r (State r)
initialState players = do
    marbles <- clSingleton 0
    pure $ State {
        marbles,
        scores: caReplicate players (Long.fromInt 0)
    }

placeMarble :: forall r. Int -> Int -> State r -> ST r (State r)
placeMarble turn marbleScore (State { marbles, scores }) =
    if marbleScore `mod` 23 == 0 then do
        marbles' <- clAdvance (-7) marbles
        marbles'' <- clDeleteHead marbles'
        pure $ State {
            marbles: marbles'',
            scores: caModifyAt turn (_ + (Long.fromInt (clHead marbles' # fromMaybe 0)) + (Long.fromInt marbleScore)) scores
        }
    else do
        marbles' <- clAdvance 2 marbles
        marbles'' <- clInsertHead marbleScore marbles'
        pure $ State {
            marbles: marbles'',
            scores
        }

takeTurn :: forall r. State r -> Int -> ST r (State r)
takeTurn state turn = placeMarble turn (turn + 1) state

game :: forall r. Int -> Int -> ST r (Array (State r))
game players lastMarbleScore = do
    init <- initialState players
    Array.foldM go [init] (Array.range 0 (lastMarbleScore - 1))
    where
        go :: Array (State r) -> Int -> ST r (Array (State r))
        go states turn = do
            let state = unsafePartial $ Array.last states # fromJust
            newState <- takeTurn state turn
            pure $ Array.snoc states newState

play :: forall r. Int -> Int -> ST r (State r)
play players turns = do
    init <- initialState players
    Array.foldRecM takeTurn init (Array.range 0 (turns - 1))

winningScore :: Int -> Int -> Maybe Long
winningScore players lastMarbleScore = ST.run do
    State finalState <- play players lastMarbleScore
    pure $ finalState.scores # maximum