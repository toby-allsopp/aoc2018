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
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (toCharArray)
import Data.String.Yarn (lines)
import Data.Traversable (traverse_)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial, unsafeCrashWith)

type Map = Array (Array Char)

data Direction = DirUp | DirRight | DirDown | DirLeft | Crashed

derive instance eqDirection :: Eq Direction

instance showDirection :: Show Direction where
    show DirUp = "^"
    show DirRight = ">"
    show DirDown = "v"
    show DirLeft = "<"
    show Crashed = "X"

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
turnCorner '/' Crashed = Crashed
turnCorner '\\' DirUp = DirLeft
turnCorner '\\' DirDown = DirRight
turnCorner '\\' DirRight = DirDown
turnCorner '\\' DirLeft = DirUp
turnCorner '\\' Crashed = Crashed
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
turn _ Crashed = Crashed

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
            Crashed -> { newX: x, newY: y }
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

fromJustWithReason :: forall a. Partial => (Unit -> String) -> Maybe a -> a
fromJustWithReason reason Nothing = unsafeCrashWith (reason unit)
fromJustWithReason _ (Just x) = x

swap :: forall r a. Partial => Int -> Int -> STArray r a -> ST r Unit
swap i j a = do
    x <- STArray.peek i a <#> fromJustWithReason (\_ -> "i = " <> show i <> " is out of range")
    y <- STArray.peek j a <#> fromJustWithReason (\_ -> "j = " <> show j <> " is out of range")
    STArray.poke i y a <#> if _ then unit else unsafeCrashWith $ "i = " <> show i <> " is out of range"
    STArray.poke j x a <#> if _ then unit else unsafeCrashWith $ "j = " <> show j <> " is out of range"

-- | Move elements of `a` such that the element at `j` ends up at index `i`
rotate :: forall r a. Partial => Int -> Int -> STArray r a -> ST r Unit
rotate i j a =
    case i `compare` j of
        EQ -> pure unit
        LT -> swap (j - 1) j a *> rotate i (j - 1) a
        GT -> swap j (j + 1) a *> rotate i (j + 1) a

modifyAtSortedBy :: forall r a. Partial => Eq a => Show a => (a -> a -> Ordering) -> Int -> a -> STArray r a -> ST r Int
modifyAtSortedBy cmp i x a = do
    frozen <- STArray.unsafeFreeze a
    -- pure $ if frozen == Array.sortBy cmp frozen  then unit else unsafeCrashWith $ "not sorted: " <> show frozen
    pure $ if i < Array.length frozen then unit else unsafeCrashWith $ "i = " <> show i <> " is out of range for " <> show frozen
    let newIndex = lowerBoundBy cmp x frozen # \ni -> if ni <= i then ni else ni - 1
    void $ STArray.poke i x a
    rotate newIndex i a
    pure newIndex

markCollisionAt :: forall r. Int -> STArray r Cart -> ST r Unit
markCollisionAt i carts = void $ STArray.modify i (\(Cart c) -> Cart (c { direction = Crashed })) carts

markCollisionsAround :: forall r. Int -> STArray r Cart -> ST r Unit
markCollisionsAround index carts = do
    l <- STArray.peek (index - 1) carts
    c <- STArray.peek index carts
    r <- STArray.peek (index + 1) carts
    case compareCarts <$> l <*> c of
        Just EQ -> markCollisionAt (index - 1) carts *> markCollisionAt index carts
        _ -> pure unit
    case compareCarts <$> c <*> r of
        Just EQ -> markCollisionAt (index + 1) carts *> markCollisionAt index carts
        _ -> pure unit

removeCollisionAt :: forall r. Int -> STArray r Cart -> ST r Unit
removeCollisionAt index carts = do
    c <- STArray.peek index carts
    when (fromMaybe false $ isCrashed <$> c) $ void $ STArray.splice index 1 [] carts

removeCollisionsAround :: forall r. Int -> STArray r Cart -> ST r Unit
removeCollisionsAround index carts = do
    removeCollisionAt (index + 1) carts
    removeCollisionAt index carts
    removeCollisionAt (index - 1) carts

tick :: forall a. Map -> (State -> Maybe a) -> State -> Either a State
tick map stop { carts } = { carts: _ } <$> ST.run (runExceptT go)
    where
    go :: forall r. ExceptT a (ST r) (Array Cart)
    go = do
        newCarts <- lift $ STArray.thaw carts
        traverse_ (tickCart newCarts) carts
        lift $ STArray.unsafeFreeze newCarts
        where
        tickCart :: STArray r Cart -> Cart -> ExceptT a (ST r) Unit
        tickCart newCarts cart = do
            frozenNewCarts <- lift $ STArray.unsafeFreeze newCarts
            let currentIndex = lowerBoundBy compareCarts cart frozenNewCarts
            when (frozenNewCarts !! currentIndex <#> compareCarts cart <#> (_ == EQ) # fromMaybe false) do
                let cart' = moveCart map cart
                newIndex <- lift $ unsafePartial $ modifyAtSortedBy compareCarts currentIndex cart' newCarts
                lift $ markCollisionsAround newIndex newCarts
                frozen2 <- lift $ STArray.unsafeFreeze newCarts
                case stop { carts: frozen2 } of
                    Just result -> throwError result
                    Nothing -> do
                        lift $ removeCollisionsAround newIndex newCarts

runUntil :: forall a. (State -> Maybe a) -> (State -> Maybe a) -> Map -> State -> a
runUntil stopWithinTick stopAfterTick map = go
    where
    go :: State -> a
    go state = case tick map stopWithinTick state of
        Left collision -> collision
        Right nextState ->
            case stopAfterTick nextState of
                Just collision -> collision
                Nothing -> go nextState

isCrashed :: Cart -> Boolean
isCrashed (Cart { direction }) = direction == Crashed

type Collision = { x :: Int, y :: Int }

findCollision :: State -> Maybe Collision
findCollision { carts } =
    Array.filter isCrashed carts
        # Array.head
        <#> (\element -> { x: cartX element, y: cartY element })

firstCrash :: Map -> State -> Collision
firstCrash = runUntil findCollision (const Nothing)

oneCartRemaining :: State -> Maybe Cart
oneCartRemaining { carts } = if Array.length carts == 1 then Array.head carts else Nothing

lastRemainingCart :: Map -> State -> Cart
lastRemainingCart = runUntil (const Nothing) oneCartRemaining