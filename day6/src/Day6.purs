module Day6 where

import Prelude (Ordering(..), Unit, bind, compare, discard, div, mod, pure, void, (#), ($), (*), (+), (-), (/), (<), (<#>), (<<<), (==), (>), (>=), (||))

import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Array.NonEmpty as ANE
import Data.Foldable (class Foldable, foldl, maximum)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Data.Ord (abs)
import Partial.Unsafe (unsafePartial)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.String.Yarn (lines)
import Parser (eof, integer, literal, runParser)
import Data.Traversable (sequence)
import Data.Either (Either)
import Data.Unfoldable (unfoldr)
import Data.NonEmpty (NonEmpty, (:|))
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex)

type Coord = { x :: Int, y :: Int }

parseLine :: String -> Either String Coord
parseLine = runParser do
    x <- integer
    _ <- literal ", "
    y <- integer
    eof
    pure {x, y}

parseInput :: String -> Either String (Array Coord)
parseInput input = lines input <#> parseLine # sequence

type Cell = {
    closestCoordIndex :: Maybe Int, -- Nothing if more than one coord is closest
    closestCoordDistance :: Int
}

data Array2D a = Array2D {
    a :: Array a,
    cols :: Int
}

array2dOf :: forall a. a -> Int -> Int -> Array2D a
array2dOf e x y = Array2D { a : Array.replicate (x * y) e, cols: x }

nextCoord :: Coord -> Int -> Coord
nextCoord {x, y} cols =
    let nextX = if x < cols - 1 then x + 1 else 0 in
    let nextY = if nextX > 0 then y else y + 1 in
    {x : nextX, y : nextY}

array2dGenerate :: forall a. Int -> Int -> (Int -> Int -> a) -> Array2D a
array2dGenerate cols rows f = Array2D { a : unfoldr g {x:0, y:0}, cols }
    where
        g :: Coord -> Maybe (Tuple a Coord)
        g {x, y} =
            if y >= cols then
                Nothing
            else
                Just $ Tuple (f x y) (nextCoord {x, y} cols)

foldli :: forall f a b. Foldable f => (b -> Int -> a -> b) -> b -> f a -> b
foldli f init = snd <<< foldl (\(Tuple i value) x -> Tuple (i + 1) (f value i x)) (Tuple 0 init)

array2dFoldi :: forall a b. (b -> Int -> Int -> a -> b) -> b -> Array2D a -> b
array2dFoldi f init (Array2D {a, cols}) = array2dFoldi' 0 0 init
    where
        array2dFoldi' x y value =
            let i = x + y * cols in
            if i < Array.length a then
                let nextX = if x < cols - 1 then x + 1 else 0 in
                let nextY = if nextX > 0 then y else y + 1 in
                array2dFoldi' nextX nextY (f value x y (unsafePartial $ Array.unsafeIndex a i))
            else
                value

type Board = Array2D Cell

emptyBoard :: Board
emptyBoard = Array2D {a: [], cols: 0}

data Array2DST h a = Array2DST {
    a :: STArray h a,
    cols :: Int
}

withArray2DST :: forall a h. Array2D a -> (Array2DST h a -> ST h Unit) -> ST h (Array2D a)
withArray2DST (Array2D {a, cols}) f = do
    a' <- STArray.withArray (\sta -> f (Array2DST {a: sta, cols})) a
    pure $ Array2D {a: a', cols}

array2dSTModify :: forall a h. Array2DST h a -> (Int -> Int ->  a -> a) -> ST h Unit
array2dSTModify (Array2DST {a, cols}) f = do
    aa <- STArray.toAssocArray a
    ST.foreach aa $ \{value, index} ->
        let x = index `mod` cols in
        let y = index `div` cols in
        void $ STArray.poke index (f x y value) a

isEdge :: forall a. Array2D a -> Int -> Int -> Boolean
isEdge _ 0 _ = true
isEdge _ _ 0 = true
isEdge (Array2D {a, cols}) x y = x == cols - 1 || y == ((Array.length a) / cols)- 1

minimaxNonEmptyBy :: forall a f. Foldable f => (a -> a -> Ordering) -> NonEmpty f a -> {min :: a, max :: a}
minimaxNonEmptyBy cmp (x :| xs)= foldl minimax' {min: x, max: x} xs
  where
  minimax' {min, max} y = { min: (if cmp min y == LT then min else y)
                          , max: (if cmp max y == GT then max else y)
                          }

foreachi :: forall h a. Array a -> (Int -> a -> ST h Unit) -> ST h Unit
foreachi a f = ST.for 0 (Array.length a) $ \i -> f i (unsafePartial $ Array.unsafeIndex a i)

type BoardST h = Array2DST h (Maybe Cell)

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance {x: x1, y: y1} {x: x2, y:y2} = abs (x2 - x1) + abs (y2 - y1)

foldlNonEmptyWithIndex :: forall f a b. FoldableWithIndex Int f => (Int -> Maybe b -> a -> b) -> NonEmpty f a -> b
foldlNonEmptyWithIndex f (x :| xs) = foldlWithIndex (\i v e -> f (i + 1) (Just v) e) (f 0 Nothing x) xs

findClosestCoords :: Array Coord -> Board
findClosestCoords coords = case ANE.fromArray coords of
    Nothing -> emptyBoard
    Just coords' ->
        let coords = ANE.toNonEmpty coords' in
        let {min: minX, max: maxX} = coords <#> (_.x) # minimaxNonEmptyBy compare in
        let {min: minY, max: maxY} = coords <#> (_.y) # minimaxNonEmptyBy compare in
        array2dGenerate (maxX - minX + 1) (maxY - minY + 1) $ \x y ->
            foldlNonEmptyWithIndex (updateCellAt (x + minX) (y + minY)) coords

    where
        updateCellAt :: Int -> Int -> Int -> Maybe Cell -> Coord -> Cell
        updateCellAt x y coordIndex cell coord =
            let distance = manhattanDistance coord {x, y} in
            case cell of
                Nothing -> {closestCoordIndex: Just coordIndex, closestCoordDistance: distance}
                Just {closestCoordIndex, closestCoordDistance} ->
                    case compare distance closestCoordDistance of
                        LT -> {closestCoordIndex: Just coordIndex, closestCoordDistance: distance}
                        EQ -> {closestCoordIndex: Nothing, closestCoordDistance: distance}
                        GT -> {closestCoordIndex, closestCoordDistance}

data Area
    = Finite Int
    | Infinite

toFinite :: Area -> Maybe Int
toFinite (Finite a) = Just a
toFinite Infinite = Nothing

collectAreas :: Board -> HashMap Int Area
collectAreas board = array2dFoldi go HashMap.empty board
    where
        go :: HashMap Int Area -> Int -> Int -> Cell -> HashMap Int Area
        go areas x y {closestCoordIndex: Nothing} = areas
        go areas x y {closestCoordIndex: Just coordIndex} = HashMap.alter (alterCell x y) coordIndex areas

        alterCell :: Int -> Int -> Maybe Area -> Maybe Area
        alterCell x y _ | isEdge board x y = Just Infinite
        alterCell x y Nothing = Just (Finite 1)
        alterCell x y (Just Infinite) = Just Infinite
        alterCell x y (Just (Finite a)) = Just (Finite (a + 1))

largestFiniteArea :: Board -> Maybe Int
largestFiniteArea board = collectAreas board # HashMap.values # Array.mapMaybe toFinite # maximum
