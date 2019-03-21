module Day13 where

import Prelude

import Control.Monad.Except.Trans (ExceptT, throwError, lift, runExceptT)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Array ((!!))
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.String.CodeUnits (toCharArray)
import Data.String.Yarn (lines)
import Data.Traversable (traverse_)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial, unsafeCrashWith)

type Map = Array (Array Char)

data Direction = DirUp | DirRight | DirDown | DirLeft

derive instance eqDirection :: Eq Direction

instance showDirection :: Show Direction where
    show DirUp = "^"
    show DirRight = ">"
    show DirDown = "v"
    show DirLeft = "<"

data Turn = Straight | TurnLeft | TurnRight

derive instance eqTurn :: Eq Turn

instance showTurn :: Show Turn where
    show Straight = "Straight"
    show TurnLeft = "TurnLeft"
    show TurnRight = "TurnRight"

newtype Cart = Cart { x :: Int, y :: Int, direction :: Direction, nextIntersectionChoice :: Turn }

derive instance eqCart :: Eq Cart

instance showCart :: Show Cart where
    show (Cart c) = show c

compareCarts :: Cart -> Cart -> Ordering
compareCarts = comparing (\(Cart {x, y}) -> [y, x])

cartX :: Cart -> Int
cartX (Cart {x}) = x

cartY :: Cart -> Int
cartY (Cart {y}) = y

type Carts = Array Cart

type State = { carts :: Carts }

processRawMap :: Map -> { map :: Map, state :: State }
processRawMap m = (runWriter go) # (\(Tuple map carts) -> { map, state: { carts: Array.sortBy compareCarts carts } })
    where
    go :: Writer Carts Map
    go = traverseWithIndex goLine m

    goLine :: Int -> Array Char -> Writer Carts (Array Char)
    goLine y line = traverseWithIndex goChar line

        where
        goChar :: Int -> Char -> Writer Carts Char
        goChar x =
            let cart direction track = tell [Cart { x, y, direction, nextIntersectionChoice: TurnLeft }] *> pure track in
            case _ of
                '^' -> cart DirUp    '|'
                '>' -> cart DirRight '-'
                'v' -> cart DirDown  '|'
                '<' -> cart DirLeft  '-'
                c -> pure c

parseInput :: String -> { map :: Map, state :: State }
parseInput s =
    let rawMap = lines s <#> toCharArray in
    processRawMap rawMap

turnCorner :: Char -> Direction -> Direction
turnCorner '/' DirUp = DirRight
turnCorner '/' DirDown = DirLeft
turnCorner '/' DirRight = DirUp
turnCorner '/' DirLeft = DirDown
turnCorner '\\' DirUp = DirLeft
turnCorner '\\' DirDown = DirRight
turnCorner '\\' DirRight = DirDown
turnCorner '\\' DirLeft = DirUp
turnCorner _ d = d

turn :: Turn -> Direction -> Direction
turn Straight d = d
turn TurnLeft DirUp = DirLeft
turn TurnLeft DirRight = DirUp
turn TurnLeft DirDown = DirRight
turn TurnLeft DirLeft = DirDown
turn TurnRight DirUp = DirRight
turn TurnRight DirRight = DirDown
turn TurnRight DirDown = DirLeft
turn TurnRight DirLeft = DirUp

cycleNextIntersectionChoice :: Turn -> Turn
cycleNextIntersectionChoice TurnLeft = Straight
cycleNextIntersectionChoice Straight = TurnRight
cycleNextIntersectionChoice TurnRight = TurnLeft

moveCart :: Map -> Cart -> Cart
moveCart map cart@(Cart { x, y, direction, nextIntersectionChoice }) =
    let { newX, newY } = case direction of
            DirUp -> { newX: x, newY: y - 1 }
            DirRight -> { newX: x + 1, newY: y }
            DirDown -> { newX: x, newY: y + 1 }
            DirLeft -> { newX: x - 1, newY: y }
            in
    case map !! newY of
        Nothing -> unsafeCrashWith $ "moved off map: " <> show newX <> "," <> show newY
        Just line ->
            case line !! newX of
                Nothing -> unsafeCrashWith $ "moved off map: " <> show newX <> "," <> show newY
                Just c -> case c of
                    '-' -> Cart { x: newX, y: newY, direction, nextIntersectionChoice }
                    '|' -> Cart { x: newX, y: newY, direction, nextIntersectionChoice }
                    '/' -> Cart { x: newX, y: newY, direction: turnCorner c direction, nextIntersectionChoice }
                    '\\' -> Cart { x: newX, y: newY, direction: turnCorner c direction, nextIntersectionChoice }
                    '+' -> Cart { x: newX, y: newY, direction: turn nextIntersectionChoice direction, nextIntersectionChoice: cycleNextIntersectionChoice nextIntersectionChoice }
                    _ -> unsafeCrashWith $ "unexpected track char at " <> show newX <> "," <> show newY <> ": " <> show c

-- | Find the index in a sorted array at which an element can be inserted preserving the order.
-- | If there are multiple elements in the array that are equal to `x` then the chosen index is unspecified.
lowerBoundBy :: forall a. (a -> a -> Ordering) -> a -> Array a -> Int
lowerBoundBy cmp x a = go 0 (Array.length a)
    where
    go :: Int -> Int -> Int
    go l u | l == u = l
    go l u | otherwise =
        let i = (l + u) `div` 2 in
        case unsafePartial $ Array.unsafeIndex a i `cmp` x of
            EQ -> i
            LT -> go (i + 1) u
            GT -> go l i

swap :: forall r a. Partial => Int -> Int -> STArray r a -> ST r Unit
swap i j a = do
    x <- STArray.peek i a <#> fromJust
    y <- STArray.peek j a <#> fromJust
    void $ STArray.poke i y a
    void $ STArray.poke j x a

-- | Move elements of `a` such that the element at `j` ends up at index `i`
rotate :: forall r a. Partial => Int -> Int -> STArray r a -> ST r Unit
rotate i j a =
    case i `compare` j of
        EQ -> pure unit
        LT -> swap (j - 1) j a *> rotate i (j - 1) a
        GT -> swap j (j + 1) a *> rotate i (j + 1) a

modifyAtSortedBy :: forall r a. Partial => (a -> a -> Ordering) -> Int -> a -> STArray r a -> ST r a
modifyAtSortedBy cmp i x a = do
    frozen <- STArray.unsafeFreeze a
    let newIndex = lowerBoundBy cmp x frozen # \ni -> if ni < Array.length frozen then ni else ni - 1
    elementAtNewIndex <- STArray.peek newIndex a
    void $ STArray.poke i x a
    case elementAtNewIndex of
        Just e -> unsafePartial $ rotate newIndex i a *> pure e
        Nothing -> pure $ unsafeCrashWith "what?" --unsafePartial $ rotate (newIndex - 1) i a >>= pure (newIndex - 1)
    -- pure elementAtNewIndex

type Collision = { x :: Int, y :: Int }

tick :: Map -> State -> Either Collision State
tick map { carts } = { carts: _ } <$> ST.run (runExceptT go)
    where
    go :: forall r. ExceptT Collision (ST r) (Array Cart)
    go = do
        newCarts <- lift $ STArray.thaw carts
        traverse_ (tickCart newCarts) carts
        lift $ STArray.unsafeFreeze newCarts
        where
        tickCart :: STArray r Cart -> Cart -> ExceptT Collision (ST r) Unit
        tickCart newCarts cart = do
            frozenNewCarts <- lift $ STArray.unsafeFreeze newCarts
            let currentIndex = unsafePartial $ fromJust $ Array.findIndex (compareCarts cart >>> (_ == EQ)) frozenNewCarts
            let cart' = moveCart map cart
            collider <- lift $ unsafePartial $ modifyAtSortedBy compareCarts currentIndex cart' newCarts
            if compareCarts cart' collider == EQ then throwError { x: cartX cart', y: cartY cart' } else pure unit

firstCrash :: Map -> State -> Collision
firstCrash map = go
    where
    go :: State -> Collision
    go state = case tick map state of
        Left collision -> collision
        Right nextState -> go nextState