module Day15 where

import Array2d
import Control.Monad.Writer.Trans
import Position
import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Apply (lift2)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Data.Array (foldMap, foldl, foldr, (!!))
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldM, maximum, minimumBy, sum)
import Data.Function (on)
import Data.Function.Memoize (class Tabulate, tabulate, memoize)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)
import Data.String.CodeUnits as String
import Data.String.Yarn (lines, unlines)
import Data.Traversable (class Traversable, sequence, traverse, sequenceDefault, traverse_)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Parser as P
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import ShortestPaths as SP
import Debug (debug)

readingOrder :: Position -> Position -> Ordering
readingOrder (Position p1) (Position p2) = [p1.y, p1.x] `compare` [p2.y, p2.x]

upStep :: Position
upStep = makePosition 0 (-1)

rightStep :: Position
rightStep = makePosition 1 0

downStep :: Position
downStep = makePosition 0 1

leftStep :: Position
leftStep = makePosition (-1) 0

data Square = Wall | Open

instance showSquare :: Show Square where
    show Wall = "#"
    show Open = "."

squareParser :: P.Parser Char Square
squareParser = do
    c <- P.char
    case c of
        '#' -> pure Wall
        '.' -> pure Open
        _ -> P.fail $ "invalid square: " <> show c

parseSquare :: forall m. MonadThrow String m => Char -> m Square
parseSquare '#' = pure Wall
parseSquare '.' = pure Open
parseSquare c = throwError $ "invalid square: " <> show c

parseUnitType :: forall m. MonadThrow String m => Char -> m UnitType
parseUnitType 'E' = pure Elf
parseUnitType 'G' = pure Goblin
parseUnitType c = throwError $ "invalid unit type: " <> show c

parseUnit :: forall m. MonadThrow String m => Position -> Char -> m UnitState
parseUnit p c = flip makeUnit p <$> parseUnitType c

parseSquareOrUnit :: forall m. MonadThrow String m => Alt m => Position -> Char -> WriterT (Array UnitState) m Square
parseSquareOrUnit p c = lift (parseSquare c) <|> do
    u <- lift $ parseUnit p c
    tell [u]
    lift (pure Open)

type Map = Array2d Square

parseMap :: forall m. MonadThrow String m => Alt m => String -> m { map :: Map, units :: Units }
parseMap s = runWriterT (traverseWithIndex parseSquareOrUnit =<< (lift $ parseArray2d s))
    <#> \(Tuple map units) -> { map, units: Units units }

squareAt :: Position -> Map -> Maybe Square
squareAt = index2d

data UnitType = Goblin | Elf

derive instance eqUnitType :: Eq UnitType

instance showUnitType :: Show UnitType where
    show Goblin = "G"
    show Elf    = "E"

newtype UnitState = UnitState {
    unitType :: UnitType,
    position :: Position,
    attackPower :: Int,
    hitPoints :: Int
}

derive instance eqUnitState :: Eq UnitState

instance showUnitState :: Show UnitState where
    show (UnitState u) = show u

makeUnit :: UnitType -> Position -> UnitState
makeUnit unitType position = UnitState { unitType, position, attackPower: 3, hitPoints: 200 }

unitPosition :: UnitState -> Position
unitPosition (UnitState u) = u.position

updateUnitPosition :: UnitState -> Position -> UnitState
updateUnitPosition (UnitState u) p = UnitState $ u { position = p }

unitHitPoints :: UnitState -> Int
unitHitPoints (UnitState u) = u.hitPoints

damageUnitBy :: Int -> UnitState -> UnitState
damageUnitBy damage (UnitState u) = UnitState $ u { hitPoints = u.hitPoints - damage }

unitAttackPower :: UnitState -> Int
unitAttackPower (UnitState u) = u.attackPower

isDead :: UnitState -> Boolean
isDead (UnitState u) = u.hitPoints <= 0

isEnemy :: UnitState -> UnitState -> Boolean
isEnemy (UnitState u1) (UnitState u2) = u1.unitType /= u2.unitType

newtype Units = Units (Array UnitState)

derive instance eqUnits :: Eq Units

instance showUnits :: Show Units where
    show (Units units) = show units

unitsToArray :: Units -> Array UnitState
unitsToArray (Units units) = units

foldrUnits :: forall b. (UnitState -> b -> b) -> b -> Units -> b
foldrUnits f i (Units u) = foldr f i u

foldlUnits :: forall b. (b -> UnitState -> b) -> b -> Units -> b
foldlUnits f i (Units u) = foldl f i u

foldMUnits :: forall m b. Monad m => (b -> UnitState -> m b) -> b -> Units -> m b
foldMUnits f i (Units u) = foldM f i u

unitAt :: Position -> Units -> Maybe UnitState
unitAt p (Units units) = Array.filter (unitPosition >>> eq p) units # Array.head

removeUnitAt :: Position -> Units -> Units
removeUnitAt p (Units units) = Units $ Array.filter (unitPosition >>> not eq p) units

updateUnitAt :: Position -> UnitState -> Units -> Units
updateUnitAt p u (Units units) =
    case Array.findIndex (unitPosition >>> eq p) units of
        Just i -> Units $ unsafePartial $ fromJust $ Array.updateAt i u units
        Nothing -> Units units

targetsOfUnit :: Units -> UnitState -> Array UnitState
targetsOfUnit (Units units) unit = Array.filter (isEnemy unit) units

adjacentPositions :: Position -> Array Position
adjacentPositions p = (p + _) <$> [upStep, rightStep, downStep, leftStep]

type UnitOrSquare = Either Square UnitState

unitOrSquareAt :: Map -> Units -> Position -> Maybe UnitOrSquare
unitOrSquareAt map units p = (Right <$> unitAt p units) <|> (Left <$> squareAt p map)

isOpenOrPosition :: Position -> UnitOrSquare -> Boolean
isOpenOrPosition p (Right u) | unitPosition u == p = true
isOpenOrPosition _ (Left Open)                     = true
isOpenOrPosition _ _                               = false

inRangeOfTarget :: UnitState -> Map -> Units -> Array UnitState -> Array Position
inRangeOfTarget unit map units targets =
    targets
    >>= unitPosition >>> adjacentPositions
    # Array.filter (\p -> unitOrSquareAt map units p <#> isOpenOrPosition (unitPosition unit) # fromMaybe false)

isOpen :: UnitOrSquare -> Boolean
isOpen (Left Open) = true
isOpen _           = false

openAdjacentPositions :: Map -> Units -> Position -> Array Position
openAdjacentPositions map units =
    adjacentPositions
    >>> Array.filter (\p -> unitOrSquareAt map units p <#> isOpen # fromMaybe false)

labelShortestPathsTo :: Map -> Units -> Position -> Array2d (Maybe SP.Label)
labelShortestPathsTo map units to =
    debug ("labelShortestPathsTo " <> show to) $ \_ ->
    SP.labelShortestPathsTo (array2dCols map) (array2dRows map) (openAdjacentPositions map units) to

nextSteps :: Array (Array Position) -> Array Position
nextSteps = Array.mapMaybe (\path -> path !! (Array.length path - 2))

nextStep :: Array (Array Position) -> Maybe Position
nextStep = nextSteps >>> minimumBy readingOrder

spPathFrom :: SP.Path -> Maybe Position
spPathFrom = _.paths >>> Array.head >=> Array.head

chainCompare :: forall a. (a -> a -> Ordering) -> (a -> a -> Ordering) -> a -> a -> Ordering
chainCompare cmp1 cmp2 x y =
    case cmp1 x y of
        EQ -> cmp2 x y
        o -> o

compareDistanceThenReadingOrder :: SP.Path -> SP.Path -> Ordering
compareDistanceThenReadingOrder =
    chainCompare (compare `on` _.distance) (\x y -> fromMaybe EQ $ (lift2 readingOrder `on` spPathFrom) x y)

updateUnitInUnits :: UnitState -> UnitState -> Units -> Units
updateUnitInUnits oldUnit newUnit (Units units) =
    case Array.findIndex (\u -> unitPosition u == unitPosition oldUnit) units of
        Nothing -> Units units
        Just index -> Units $ fromMaybe units $ Array.sortBy (readingOrder `on` unitPosition) <$> Array.updateAt index newUnit units

move :: Map -> UnitState -> Units -> Maybe UnitState
move map unit units =
    debug ("move " <> show unit) $ \_ ->
    let sps = labelShortestPathsTo map units (unitPosition unit) in
    let targets = targetsOfUnit units unit in
    if Array.null targets then
        Nothing
    else
        let inRange = inRangeOfTarget unit map units targets in
        let pathsToNearestInRange = minimumBy compareDistanceThenReadingOrder $ Array.catMaybes $ (flip SP.shortestPathsFrom sps <$> inRange) in
        let movedUnit = pathsToNearestInRange <#> _.paths >>= nextStep <#> updateUnitPosition unit # fromMaybe unit in
        Just movedUnit

adjacentTargets :: Map -> UnitState -> Units -> Array UnitState
adjacentTargets map unit units =
    adjacentPositions (unitPosition unit)
    <#> flip unitAt units
    # Array.catMaybes
    # Array.filter (isEnemy unit)

compareHitPointsThenReadingOrder :: UnitState -> UnitState -> Ordering
compareHitPointsThenReadingOrder = chainCompare (compare `on` unitHitPoints) (readingOrder `on` unitPosition)

attack :: Map -> UnitState -> Units -> Units
attack map unit units =
    debug ("attack " <> show unit) $ \_ ->
    let targets = adjacentTargets map unit units in
    let weakest = minimumBy compareHitPointsThenReadingOrder targets in
    let damagedUnit = damageUnitBy (unitAttackPower unit) <$> weakest in
    case damagedUnit of
        Nothing -> units
        Just du ->
            if isDead du then
                removeUnitAt (unitPosition du) units
            else
                updateUnitAt (unitPosition du) du units

turn :: Map -> UnitState -> Units -> Either Units Units
turn map unit units =
    debug ("turn " <> show unit) $ \_ ->
    case unitAt (unitPosition unit) units of
        Nothing -> Right units
        Just currentUnit ->
            case move map currentUnit units of
                Nothing -> Left units
                Just movedUnit ->
                    let movedUnits = updateUnitInUnits currentUnit movedUnit units in
                    Right $ attack map movedUnit movedUnits

round :: Map -> Units -> Either Units Units
round map units = foldMUnits (flip (turn map)) units units

rounds :: Int -> Map -> Units -> Either Units Units
rounds 0 map units = Right units
rounds n map units = rounds (n - 1) map =<< (round map units)

battle :: Map -> Units -> { rounds :: Int, units :: Units }
battle map = go 0
    where
    go n units = debug ("Round " <> show n) $ \_ -> case round map units of
        Right newUnits -> go (n + 1) newUnits
        Left newUnits -> { rounds: n, units: newUnits }

outcome :: { rounds :: Int, units :: Units } -> Int
outcome { rounds, units } = rounds * (foldlUnits (\sum unit -> sum + unitHitPoints unit) 0 units)