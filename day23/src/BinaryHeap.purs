module BinaryHeap (
    BinaryHeap,
    empty,
    extractMin,
    insert
) where

import Prelude

import Control.Monad.ST (ST)
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Maybe (Maybe(..))

newtype BinaryHeap r a = BinaryHeap { a :: STArray r a, cmp :: a -> a -> Ordering }

empty :: forall r a. (a -> a -> Ordering) -> ST r (BinaryHeap r a)
empty cmp = do
    a <- STArray.empty
    pure (BinaryHeap { a, cmp })

extractMin :: forall r a. BinaryHeap r a -> ST r (Maybe a)
extractMin (BinaryHeap {a, cmp}) = do
    minElement <- STArray.peek 0 a
    lastElement <- STArray.pop a
    case lastElement of
        Nothing -> pure unit
        Just le -> do
            ok <- STArray.poke 0 le a
            downHeap cmp 0 a
    pure minElement

children :: Int -> { left :: Int, right :: Int }
children i = { left: i * 2 + 1, right: i * 2 + 2 }

parent :: Int -> Int
parent i = (i - 1) / 2

downHeap :: forall r a. (a -> a -> Ordering) -> Int -> STArray r a -> ST r Unit
downHeap cmp i a = do
    let {left, right} = children i
    let smallest = i
    aleft <- STArray.peek left a
    asmallest <- STArray.peek smallest a
    let smallest' = case aleft, asmallest of
            Just l, Just s | cmp l s == LT -> left
            _, _ -> smallest
    asmallest' <- STArray.peek smallest' a
    aright <- STArray.peek right a
    let smallest'' = case aright, asmallest' of
            Just r, Just s | cmp r s == LT -> right
            _, _ -> smallest'
    when (smallest'' /= i) do
        swap i smallest'' a
        downHeap cmp smallest'' a

swap :: forall r a. Int -> Int -> STArray r a -> ST r Unit
swap i j a = do
    ai <- STArray.peek i a
    aj <- STArray.peek j a
    case ai, aj of
        Just jai, Just jaj -> do
            void $ STArray.poke i jaj a
            void $ STArray.poke j jai a
        _, _ -> pure unit

insert :: forall r a. a -> BinaryHeap r a -> ST r Unit
insert x (BinaryHeap {a, cmp}) = do
    length <- STArray.push x a
    upHeap cmp (length - 1) a

upHeap :: forall r a. (a -> a -> Ordering) -> Int -> STArray r a -> ST r Unit
upHeap cmp i a = do
    let p = parent i
    ai <- STArray.peek i a
    ap <- STArray.peek p a
    case ai, ap of
        Just jai, Just jap | cmp jai jap == LT -> do
            swap i p a
            upHeap cmp p a
        _, _ -> pure unit