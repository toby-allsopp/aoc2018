module Position where

import Prelude

import Data.Hashable (class Hashable, hash)
import Data.Ord (abs)
import Partial.Unsafe (unsafeCrashWith)

newtype Position = Position { x :: Int, y :: Int }

derive instance eqPosition :: Eq Position

instance hashablePosition :: Hashable Position where
    hash (Position p) = hash p

instance showPosition :: Show Position where
    show (Position p) = show p

instance semiringPosition :: Semiring Position where
    add (Position p1) (Position p2) = Position { x: p1.x + p2.x, y: p1.y + p2.y }
    zero = Position { x: 0, y: 0 }
    mul (Position p1) (Position p2) = unsafeCrashWith "don't multiply positions!"
    one = Position one

makePosition :: Int -> Int -> Position
makePosition x y = Position { x, y }

positionX :: Position -> Int
positionX (Position p) = p.x

positionY :: Position -> Int
positionY (Position p) = p.y

manhattanDistance :: Position -> Position -> Int
manhattanDistance (Position p1) (Position p2) = (abs (p1.x - p2.x)) + (abs (p1.y - p2.y))