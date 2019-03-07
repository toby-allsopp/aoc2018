module Day10 where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Long as Long
import Data.Long (Long)
import Data.Maybe (fromMaybe)
import Data.String (joinWith)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Yarn (lines)
import Data.Traversable (foldl, sequence)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Parser as P

type Position = { x :: Int, y :: Int }
type Velocity = { dx :: Int, dy :: Int }
type Point = { position :: Position, velocity :: Velocity }

type Parser = P.Parser Char
parseLine :: Parser Point
parseLine = do
    _ <- P.literal "position=<"
    _ <- P.spaces
    x <- P.integer
    _ <- P.literal ","
    _ <- P.spaces
    y <- P.integer
    _ <- P.literal "> velocity=<"
    _ <- P.spaces
    dx <- P.integer
    _ <- P.literal ","
    _ <- P.spaces
    dy <- P.integer
    _ <- P.literal ">"
    P.eof
    pure { position: { x, y }, velocity: { dx, dy } }

parseInput :: String -> Either String (Array Point)
parseInput s = lines s <#> P.runParser parseLine # sequence

posPlusVel :: Position -> Velocity -> Position
posPlusVel { x, y } { dx, dy } = { x: x + dx, y: y + dy }

positionMinus :: Position -> Position -> Position
positionMinus p1 p2 = { x: p1.x - p2.x, y: p1.y - p2.y }

step :: Array Point -> Array Point
step points = points <#> (\{ position, velocity } -> { position: position `posPlusVel` velocity, velocity })

bounds :: Array Point -> { minPos :: Position, maxPos :: Position }
bounds = foldl go { minPos: { x: top, y: top }, maxPos: { x: bottom, y: bottom } }
    where
    go { minPos, maxPos } { position } = {
        minPos: { x: min minPos.x position.x, y: min minPos.y position.y },
        maxPos: { x: max maxPos.x position.x, y: max maxPos.y position.y }
    }

boundsSize :: { minPos :: Position, maxPos :: Position } -> Long
boundsSize { minPos, maxPos } =
    let result = (Long.fromInt (maxPos.x - minPos.x)) * (Long.fromInt (maxPos.y - minPos.y)) in
    if result <= Long.fromInt 0 then unsafeCrashWith ("OMG: " <> show { minPos, maxPos })
    else result

runUntilMinBounds :: Array Point -> { points :: Array Point, steps :: Int }
runUntilMinBounds initialPoints = go top { points: initialPoints, steps: 0 }
    where
    go lastBoundsSize { points: lastPoints, steps } =
        let nextPoints = step lastPoints in
        let nextBoundsSize = boundsSize (bounds nextPoints) in
        if nextBoundsSize > lastBoundsSize then
            { points: lastPoints, steps }
        else
            go nextBoundsSize { points: nextPoints, steps: steps + 1 }

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

array2dUpdateAt :: forall a. Int -> Int -> a -> Array2D a -> Array2D a
array2dUpdateAt x y v (Array2D { a, cols }) = Array2D { a: Array.updateAt (x + y * cols) v a # fromMaybe a, cols }

pointsTo2d :: Array Point -> Array2D Boolean
pointsTo2d points =
    let  { minPos, maxPos } = bounds points in
    foldl go (array2dOf false (maxPos.x - minPos.x + 1) (maxPos.y - minPos.y + 1)) (points <#> (_.position) <#> (_ `positionMinus` minPos))
    where
        go a { x, y } = array2dUpdateAt x y true a

array2dRows :: forall a. Array2D a -> Array (Array a)
array2dRows (Array2D { a, cols }) = foldl row [] (Array.range 0 ((Array.length a) / cols - 1))
    where
        row rows y = Array.snoc rows (foldl (col y) [] (Array.range 0 (cols - 1)))
        col y colAccum x = Array.snoc colAccum (unsafePartial $ Array.unsafeIndex a (x + y * cols))

array2dRender :: forall a. (a -> Char) -> Array2D a -> String
array2dRender f a =
    joinWith "\n" (array2dRows a <#> \row -> row <#> f # fromCharArray)

renderPoints :: Array Point -> String
renderPoints = pointsTo2d >>> array2dRender (\p -> if p then '#' else '.')