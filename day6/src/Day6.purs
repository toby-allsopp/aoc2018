module Day6 where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as ANE
import Data.Foldable (class Foldable, foldl, maximumBy)
import Data.Maybe (Maybe(..), fromMaybe)
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

instance array2dShow :: Show a => Show (Array2D a) where
    show (Array2D insides) = "Array2D " <> show insides

instance array2dEq :: Eq a => Eq (Array2D a) where
    eq (Array2D {a: a1, cols: cols1}) (Array2D {a: a2, cols: cols2}) = eq a1 a2 && eq cols1 cols2

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
            if y >= rows then
                Nothing
            else
                Just $ Tuple (f x y) (nextCoord {x, y} cols)

foldli :: forall f a b. Foldable f => (b -> Int -> a -> b) -> b -> f a -> b
foldli f init = snd <<< foldl (\(Tuple i value) x -> Tuple (i + 1) (f value i x)) (Tuple 0 init)

array2dFoldi :: forall a b. (b -> Int -> Int -> a -> b) -> b -> Array2D a -> b
array2dFoldi f init (Array2D {a, cols}) = array2dFoldi' 0 0 0 init
    where
        array2dFoldi' i x y value =
            if i < Array.length a then
                let nextX = if x < cols - 1 then x + 1 else 0 in
                let nextY = if nextX > 0 then y else y + 1 in
                array2dFoldi' (i + 1) nextX nextY (f value x y (unsafePartial $ Array.unsafeIndex a i))
            else
                value

type Board = Array2D Cell

array2dEmpty :: forall a. Array2D a
array2dEmpty = Array2D {a: [], cols: 0}

isEdge :: forall a. Array2D a -> Int -> Int -> Boolean
isEdge _ 0 _ = true
isEdge _ _ 0 = true
isEdge (Array2D {a, cols}) x y = x == cols - 1 || y == ((Array.length a) / cols) - 1

minimaxNonEmptyBy :: forall a f. Foldable f => (a -> a -> Ordering) -> NonEmpty f a -> {min :: a, max :: a}
minimaxNonEmptyBy cmp (x :| xs)= foldl minimax' {min: x, max: x} xs
  where
  minimax' {min, max} y = { min: (if cmp min y == LT then min else y)
                          , max: (if cmp max y == GT then max else y)
                          }

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance {x: x1, y: y1} {x: x2, y:y2} = abs (x2 - x1) + abs (y2 - y1)

foldlNonEmptyWithIndex :: forall f a b. FoldableWithIndex Int f => (Int -> Maybe b -> a -> b) -> NonEmpty f a -> b
foldlNonEmptyWithIndex f (x :| xs) = foldlWithIndex (\i v e -> f (i + 1) (Just v) e) (f 0 Nothing x) xs

generateFromCoords :: forall a. Array Coord -> (Int -> Int -> Int -> Maybe a -> Coord -> a) -> Array2D a
generateFromCoords coords f = case ANE.fromArray coords of
    Nothing -> array2dEmpty
    Just coords' ->
        let coords = ANE.toNonEmpty coords' in
        let {min: minX, max: maxX} = coords <#> (_.x) # minimaxNonEmptyBy compare in
        let {min: minY, max: maxY} = coords <#> (_.y) # minimaxNonEmptyBy compare in
        array2dGenerate (maxX - minX + 1) (maxY - minY + 1) $ \x y ->
            foldlNonEmptyWithIndex (f (x + minX) (y + minY)) coords

findClosestCoords :: Array Coord -> Board
findClosestCoords coords = generateFromCoords coords updateCellAt
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

largestFiniteArea :: Board -> Maybe {area :: Int, coordIndex :: Int}
largestFiniteArea board =
    collectAreas board
    # HashMap.toArrayBy (\k v -> {coordIndex: k, area: v})
    # Array.mapMaybe (\{coordIndex, area} -> toFinite area <#> {coordIndex, area: _})
    # maximumBy (comparing (_.area))

sumDistancesToEachCoord :: Array Coord -> Array2D Int
sumDistancesToEachCoord coords = generateFromCoords coords updateCellAt
    where
        updateCellAt :: Int -> Int -> Int -> Maybe Int -> Coord -> Int
        updateCellAt x y coordIndex sum coord =
            let distance = manhattanDistance coord {x, y} in
                (fromMaybe 0 sum) + distance

areaWithDistanceLessThan :: Int -> Array2D Int -> Int
areaWithDistanceLessThan max (Array2D {a}) = Array.filter (_ < max) a # Array.length