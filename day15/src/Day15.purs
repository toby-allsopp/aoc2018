module Day15 where

import Array2d
import Position
import Profile
import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Apply (lift2)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Control.Monad.Writer.Trans
import Data.Array (foldMap, foldl, foldr, (!!))
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldM, maximum, minimumBy, sum)
import Data.Function (on)
import Data.Function.Memoize (class Tabulate, tabulate, memoize)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
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

derive instance eqSquare :: Eq Square

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

unitsFromArray :: Array UnitState -> Units
unitsFromArray = Units <<< HashMap.fromArrayBy unitPosition identity

parseMap :: forall m. MonadThrow String m => Alt m => String -> m { map :: Map, units :: Units }
parseMap s = runWriterT (traverseWithIndex parseSquareOrUnit =<< (lift $ parseArray2d s))
    <#> \(Tuple map units) -> { map, units: unitsFromArray units }

squareAt :: Position -> Map -> Maybe Square
squareAt p map = index2d p map

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

newtype Units = Units (HashMap Position UnitState)

derive instance eqUnits :: Eq Units

instance showUnits :: Show Units where
    show (Units units) = show units

unitsToArray :: Units -> Array UnitState
unitsToArray (Units units) = HashMap.values units # Array.sortBy (readingOrder `on` unitPosition)

foldrUnits :: forall b. (UnitState -> b -> b) -> b -> Units -> b
foldrUnits f i u = foldr f i (unitsToArray u)

foldlUnits :: forall b. (b -> UnitState -> b) -> b -> Units -> b
foldlUnits f i u = foldl f i (unitsToArray u)

foldMUnits :: forall m b. Monad m => (b -> UnitState -> m b) -> b -> Units -> m b
foldMUnits f i u = foldM f i (unitsToArray u)

unitAt :: Position -> Units -> Maybe UnitState
unitAt p (Units units) = HashMap.lookup p units --unsafePartial $ Array.findIndex (unitPosition >>> eq p) units <#> (Array.unsafeIndex units)

removeUnitAt :: Position -> Units -> Units
removeUnitAt p (Units units) = Units $ HashMap.delete p units

updateUnitAt :: Position -> UnitState -> Units -> Units
updateUnitAt p u (Units units) =
    Units $ HashMap.update (Just <<< const u) p units

targetsOfUnit :: Units -> UnitState -> Array UnitState
targetsOfUnit units unit = Array.filter (isEnemy unit) (unitsToArray units)

adjacentPositions :: Position -> Array Position
-- adjacentPositions p = (p + _) <$> [upStep, rightStep, downStep, leftStep]
adjacentPositions p = [p + upStep, p + rightStep, p + downStep, p + leftStep]

data UnitOrSquareOrNeither = JustSquare Square | JustUnit UnitState | Neither

unitOrSquareAt :: Map -> Units -> Position -> UnitOrSquareOrNeither
--unitOrSquareAt map units p = profile "unitOrSquareAt" \_ -> (Right <$> unitAt p units) <|> (Left <$> squareAt p map)
unitOrSquareAt map units p = -- profile "unitOrSquareAt" \_ ->
    case squareAt p map of
        Just s ->
            case unitAt p units of
                Just u -> JustUnit u
                Nothing -> JustSquare s
        Nothing -> Neither

isOpenOrPosition :: Position -> UnitOrSquareOrNeither -> Boolean
isOpenOrPosition p (JustUnit u) | unitPosition u == p = true
isOpenOrPosition _ (JustSquare Open)                     = true
isOpenOrPosition _ _                               = false

inRangeOfTarget :: UnitState -> Map -> Units -> Array UnitState -> Array Position
inRangeOfTarget unit map units targets =
    targets
    >>= unitPosition >>> adjacentPositions
    # Array.filter (\p -> unitOrSquareAt map units p # isOpenOrPosition (unitPosition unit))

isOpen :: UnitOrSquareOrNeither -> Boolean
isOpen (JustSquare Open) = true
isOpen _           = false

openAdjacentPositions :: Map -> Units -> Position -> Array Position
openAdjacentPositions map = -- profile "openAdjacentPostions_map" \_ ->
    let a = mapWithIndex oaps map in
    \units pos -> -- profile "openAdjacentPositions_units_pos" \_ ->
        index2d pos a # fromMaybe []
            # Array.filter (\p -> unitAt p units == Nothing)
    where
        oaps pos _ =
            adjacentPositions pos
            # Array.filter (\p -> squareAt p map == Just Open)

labelShortestPathsTo :: Map -> Units -> Position -> Array Position -> Array2d (Maybe SP.Label)
labelShortestPathsTo map = let adj = openAdjacentPositions map in \units to froms ->
    -- debug ("labelShortestPathsTo " <> show to) $ \_ ->
    SP.labelShortestPathsTo (array2dCols map) (array2dRows map) (adj units) to froms

nextStep :: Array Position -> Maybe Position
nextStep = minimumBy readingOrder

chainCompare :: forall a. (a -> a -> Ordering) -> (a -> a -> Ordering) -> a -> a -> Ordering
chainCompare cmp1 cmp2 x y =
    case cmp1 x y of
        EQ -> cmp2 x y
        o -> o

compareDistanceThenReadingOrder :: Tuple Position SP.Label -> Tuple Position SP.Label -> Ordering
compareDistanceThenReadingOrder =
    chainCompare (compare `on` (snd >>> _.distance)) (readingOrder `on` fst)

updateUnitInUnits :: UnitState -> UnitState -> Units -> Units
updateUnitInUnits oldUnit newUnit (Units units) =
    Units $ HashMap.delete (unitPosition oldUnit) units # HashMap.insert (unitPosition newUnit) newUnit
    -- case Array.findIndex (\u -> unitPosition u == unitPosition oldUnit) units of
    --     Nothing -> Units units
    --     Just index -> Units $ fromMaybe units $ Array.sortBy (readingOrder `on` unitPosition) <$> Array.updateAt index newUnit units

sequenceSnd :: forall m a b. Monad m => Tuple a (m b) -> m (Tuple a b)
sequenceSnd (Tuple x my) = my >>= \y -> pure (Tuple x y)

move :: Map -> UnitState -> Units -> Maybe UnitState
move map = let lspt = labelShortestPathsTo map in \unit units ->
    -- debug ("move " <> show unit) $ \_ ->
    let targets = targetsOfUnit units unit in
    if Array.null targets then
        Nothing
    else
        let inRange = inRangeOfTarget unit map units targets in
        let sps = lspt units (unitPosition unit) inRange in
        let (positionsAndLabelsInRange :: Array (Tuple Position SP.Label)) = Array.mapMaybe sequenceSnd $ Array.zip inRange (join <$> flip index2d sps <$> inRange) in
        let (nearestInRange :: Maybe Position) = fst <$> minimumBy compareDistanceThenReadingOrder positionsAndLabelsInRange in
        let movedUnit = nearestInRange <#> SP.followNextsUntilDistance 1 sps >>= nextStep <#> updateUnitPosition unit # fromMaybe unit in
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
    -- debug ("attack " <> show unit) $ \_ ->
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
turn map = let movemap = move map in \unit units ->
    -- debug ("turn " <> show unit) $ \_ ->
    -- profile "turn" \_ ->
    case unitAt (unitPosition unit) units of
        Nothing -> Right units
        Just currentUnit ->
            case movemap currentUnit units of
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