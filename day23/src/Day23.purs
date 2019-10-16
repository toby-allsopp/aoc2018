module Day23 where

import Debug
import Prelude

import BinaryHeap (BinaryHeap)
import BinaryHeap as BinaryHeap
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Internal (STRef)
import Data.Array (foldl)
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (maximumBy, traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Data.Ordering (invert)
import Data.Tuple (Tuple(..))
import Parser as P

type Parser = P.Parser String

data Pos = Pos Int Int Int

derive instance eqPos :: Eq Pos

instance showPos :: Show Pos where
    show (Pos x y z) = "(" <> show x <> "," <> show y <> "," <> show z <> ")"

posOnEach :: (Int -> Int) -> Pos -> Pos
posOnEach f (Pos x y z) = Pos (f x) (f y) (f z)

posOnEach2 :: (Int -> Int -> Int) -> Pos -> Pos -> Pos
posOnEach2 f (Pos x1 y1 z1) (Pos x2 y2 z2) = Pos (f x1 x2) (f y1 y2) (f z1 z2)

posOnEach3 :: (Int -> Int -> Int -> Int) -> Pos -> Pos -> Pos -> Pos
posOnEach3 f (Pos x1 y1 z1) (Pos x2 y2 z2) (Pos x3 y3 z3) = Pos (f x1 x2 x3) (f y1 y2 y3) (f z1 z2 z3)

type Bot = { pos :: Pos, r :: Int }

inputLineParser :: Parser Bot
inputLineParser = do
    _ <- P.literal "pos=<"
    x <- P.integer
    _ <- P.literal ","
    y <- P.integer
    _ <- P.literal ","
    z <- P.integer
    _ <- P.literal ">, r="
    r <- P.integer
    pure { pos: Pos x y z, r }

parseInput :: String -> Either String (Array Bot)
parseInput = P.runParser (P.separatedList (P.eol) inputLineParser)

strongest :: Array Bot -> Maybe Bot
strongest = maximumBy (comparing _.r)

inRange :: Bot -> Array Bot -> Array Bot
inRange bot = Array.filter (botInRange bot)

botInRange :: Bot -> Bot -> Boolean
botInRange b1 b2 = distance b1.pos b2.pos <= b1.r

distance :: Pos -> Pos -> Int
distance p1 p2 = let (Pos x y z) = posOnEach2 (\a b -> abs (a - b)) p1 p2 in x + y + z

bounds :: Array Bot -> Maybe { minBound :: Pos, maxBound :: Pos }
bounds = foldl f Nothing
    where
        f Nothing b = Just { minBound: b.pos, maxBound: b.pos }
        f (Just { minBound, maxBound }) b = Just { minBound: posOnEach2 min minBound b.pos, maxBound: posOnEach2 max maxBound b.pos }

type Division = { minBound :: Pos, maxBound :: Pos, bots :: Array Bot }

divide :: Division -> Array Division
divide div =
    let sz@(Pos szx szy szz) = posOnEach (_ + 1) $ posOnEach2 (-) div.maxBound div.minBound in
    let dp@(Pos dpx dpy dpz) = posOnEach (_ / 2) sz in
    do
        Tuple ax bx <- if dpx > 0 then [Tuple 0 (dpx - 1), Tuple dpx (szx - 1)] else [Tuple 0 (szx - 1)]
        Tuple ay by <- if dpy > 0 then [Tuple 0 (dpy - 1), Tuple dpy (szy - 1)] else [Tuple 0 (szy - 1)]
        Tuple az bz <- if dpz > 0 then [Tuple 0 (dpz - 1), Tuple dpz (szz - 1)] else [Tuple 0 (szz - 1)]
        let blep = posOnEach2 (+) div.minBound (Pos ax ay az)
        let blop = posOnEach2 (+) div.minBound (Pos bx by bz)
        -- let blep = posOnEach2 (+) div.minBound d
        -- let blop = posOnEach2 max blep $ posOnEach (_ - 1) $ posOnEach2 (+) blep dp
        pure $ { minBound: blep, maxBound: blop, bots: Array.filter (overlaps blep blop) div.bots }

overlaps :: Pos -> Pos -> Bot -> Boolean
overlaps p1 p2 b =
    let p = clampToBoundingBox p1 p2 b.pos in
    distance b.pos p <= b.r

clampToBoundingBox :: Pos -> Pos -> Pos -> Pos
clampToBoundingBox = posOnEach3 clamp

type State r = {
    todo :: BinaryHeap r Division,
    lowerBound :: STRef r (Maybe { num :: Int, positions :: Array Pos })
}

mostCoveredPoints :: Array Bot -> Array Pos
mostCoveredPoints bots =
    case bounds bots of
        Just {minBound, maxBound} ->
            ST.run do
                todo <- BinaryHeap.empty ((invert <<< _) <<< (comparing $ Array.length <<< _.bots))
                BinaryHeap.insert {minBound, maxBound, bots} todo
                go todo
        Nothing -> []

go :: forall r. BinaryHeap r Division -> ST r (Array Pos)
go todo = tailRecM loop Nothing
    where
    loop lb = do
        next <- BinaryHeap.extractMin todo
        -- debug (show next <> " : " <> show lb) \_ ->
        case next of
            Nothing -> debug (show lb) \_ -> pure $ Done (lb <#> _.positions # fromMaybe [])
            Just n ->
                if eq n.minBound n.maxBound then
                    case lb of
                        Just b | Array.length (n.bots) < b.num ->
                            pure $ Loop lb -- pruned
                        Just b | Array.length (n.bots) == b.num -> do
                            -- equal with lower bound, add to array
                            pure $ Loop (Just { num: b.num, positions: Array.snoc b.positions n.minBound })
                        _ -> do
                            -- new lower bound
                            pure $ Loop (Just { num: Array.length n.bots, positions: [n.minBound] })
                else
                    case lb of
                        Just b | Array.length (n.bots) < b.num ->
                            pure $ Loop lb -- pruned, don't divide
                        _ -> do
                            traverse_ (flip BinaryHeap.insert todo) (divide n)
                            pure $ Loop lb
