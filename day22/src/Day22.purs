module Day22 where

import Prelude

import Control.Monad.ST (ST, for)
import Control.Monad.ST as ST
import Data.Array ((..))
import Data.Array as Array
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), cardinality, fromEnum, toEnum, upFromIncluding)
import Data.Foldable (sum)
import Data.Function.Memoize (class Tabulate, memoize, memoize2, tabulate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Show (genericShow)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (un)
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, class Unfoldable1)
import Effect.Ref (Ref)
import Effect.Unsafe (unsafePerformEffect)
import Matrix (Matrix(..))
import Matrix as Matrix
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import STMatrix (STMatrix(..))
import STMatrix as STMatrix
import ShortestPaths (Label)
import ShortestPaths as ShortestPath

newtype Cave a = Cave (Int -> Int -> a)

caveNew :: forall a. (Int -> Int -> a) -> Cave a
caveNew calc = Cave calc

caveGet :: forall a. Pos -> Cave a -> a
caveGet (Pos p) (Cave f) = f p.x p.y

derive instance functorCave :: Functor Cave

newtype GeologicIndex = GeologicIndex Int
newtype ErosionLevel = ErosionLevel Int

derive newtype instance showErosionLevel :: Show ErosionLevel

newtype Depth = Depth Int

newtype Pos = Pos { x :: Int, y :: Int }

derive instance eqPos :: Eq Pos

instance showPos :: Show Pos where
    show (Pos p) = show p

instance hashablePos :: Hashable Pos where
    hash (Pos p) = hash p

instance tabulatePos :: Tabulate Pos where
    tabulate f (Pos p) = tabulate (\(Tuple x y) -> f (Pos {x, y})) (Tuple p.x p.y)

erosionLevels :: Depth -> Pos -> Cave ErosionLevel
erosionLevels depth (Pos targetPos) =
    let erosionLevelSlow x y =
            case x, y of
                0, 0 -> geologicIndexToErosionLevel depth $ GeologicIndex 0
                x', y' | x' == targetPos.x && y' == targetPos.y -> geologicIndexToErosionLevel depth $ GeologicIndex 0
                _, 0 -> geologicIndexToErosionLevel depth $ GeologicIndex $ x * 16807
                0, _ -> geologicIndexToErosionLevel depth $ GeologicIndex $ y * 48271
                _, _ ->
                    let (ErosionLevel el1) = erosionLevel (x - 1) y
                        (ErosionLevel el2) = erosionLevel x (y - 1)
                    in
                    geologicIndexToErosionLevel depth $ GeologicIndex $ el1 * el2
        erosionLevel = memoize2 \x y -> erosionLevelSlow x y

    in
    caveNew erosionLevel

geologicIndexToErosionLevel :: Depth -> GeologicIndex -> ErosionLevel
geologicIndexToErosionLevel (Depth depth) (GeologicIndex gi) = ErosionLevel $ (gi + depth) `mod` 20183

data RegionType = Rocky | Wet | Narrow

instance showRegionType :: Show RegionType where
    show Rocky = "."
    show Wet = "="
    show Narrow = "|"

erosionLevelsToRegionTypes :: Cave ErosionLevel -> Cave RegionType
erosionLevelsToRegionTypes = map erosionLevelToRegionType

erosionLevelToRegionType :: ErosionLevel -> RegionType
erosionLevelToRegionType (ErosionLevel el) = case el `mod` 3 of
    0 -> Rocky
    1 -> Wet
    2 -> Narrow
    x -> unsafeCrashWith (show x)

riskLevel :: Pos -> Cave RegionType -> Int
riskLevel (Pos targetPos) cave =
    do
        x <- 0 .. targetPos.x
        y <- 0 .. targetPos.y
        pure $ caveGet (Pos {x, y}) cave
    # map regionTypeToRiskLevel
    # sum

regionTypeToRiskLevel :: RegionType -> Int
regionTypeToRiskLevel Rocky = 0
regionTypeToRiskLevel Wet = 1
regionTypeToRiskLevel Narrow = 2

data Dir = Up | Down | Left | Right

derive instance eqDir :: Eq Dir
derive instance ordDir :: Ord Dir
derive instance genericDir :: Generic Dir _

instance boundedDir :: Bounded Dir where
    top = genericTop
    bottom = genericBottom

instance enumDir :: Enum Dir where
    succ = genericSucc
    pred = genericPred

data Tool = Torch | ClimbingGear | Neither

derive instance eqTool :: Eq Tool
derive instance ordTool :: Ord Tool
derive instance genericTool :: Generic Tool _

instance showTool :: Show Tool where
    show = genericShow

instance boundedTool :: Bounded Tool where
    top = genericTop
    bottom = genericBottom

instance enumTool :: Enum Tool where
    succ = genericSucc
    pred = genericPred

instance boundedEnumTool :: BoundedEnum Tool where
    cardinality = genericCardinality
    fromEnum = genericFromEnum
    toEnum = genericToEnum

instance hashableTool :: Hashable Tool where
    hash = hash <<< fromEnum

data Action = Move Dir | Equip Tool

costOfAction :: Action -> Int
costOfAction (Move _) = 1
costOfAction (Equip _) = 7

newtype State = State { position :: Pos, tool :: Tool }

derive instance eqState :: Eq State

instance showState :: Show State where
    show (State s) = show s

instance hashableState :: Hashable State where
    hash (State s) = hash s

applyAction :: Action -> State -> State
applyAction (Move d) (State s) = State (s { position = applyMove d s.position })
applyAction (Equip t) (State s) = State (s { tool = t })

applyMove :: Dir -> Pos -> Pos
applyMove Up (Pos p) = Pos {x: p.x, y: p.y - 1}
applyMove Down (Pos p) = Pos {x: p.x, y: p.y + 1}
applyMove Left (Pos p) = Pos {x: p.x - 1, y: p.y}
applyMove Right (Pos p) = Pos {x: p.x + 1, y: p.y}

isActionValid :: Partial => Cave RegionType -> State -> Action -> Boolean
isActionValid cave state =
    case _ of
        Move d -> isMoveValid cave state d
        Equip t -> isEquipValid cave state t

isMoveValid :: Partial => Cave RegionType -> State -> Dir -> Boolean
isMoveValid cave (State s) d =
    let (Pos newp) = applyMove d s.position in
    newp.x >= 0 &&
    newp.y >= 0 &&
    isToolValid (caveGet (Pos { x: newp.x, y: newp.y}) cave) s.tool

isEquipValid :: Partial => Cave RegionType -> State -> Tool -> Boolean
isEquipValid cave (State {position, tool}) newt =
    newt /= tool && isToolValid (caveGet position cave) newt

isToolValid :: RegionType -> Tool -> Boolean
isToolValid Rocky ClimbingGear = true
isToolValid Rocky Torch = true
isToolValid Wet ClimbingGear = true
isToolValid Wet Neither = true
isToolValid Narrow Torch = true
isToolValid Narrow Neither = true
isToolValid _ _ = false

allActions :: Array Action
allActions = (Move <$> upFromIncluding bottom) <> (Equip <$> upFromIncluding bottom)

validActions :: Partial => Cave RegionType -> State -> Array Action
validActions cave state = allActions # Array.filter (isActionValid cave state)

foo :: Cave RegionType -> Pos -> Maybe (Label State)
foo cave targetPos =
    let labels = ShortestPath.labelShortestPathsTo adjacentStates lowerBoundFn startState [targetState] in
    HashMap.lookup targetState labels
    where
        -- width = Matrix.width cave
        -- numTools = un Cardinality (cardinality :: Cardinality Tool)

        -- numVertices = Matrix.width cave * Matrix.height cave * numTools

        -- vertices :: Array State
        -- vertices = oldVertexToState <$> Array.range 0 (numVertices - 1)

        -- vertexToState :: Vertex -> State
        -- vertexToState v = unsafePartial $ Array.unsafeIndex vertices v

        -- oldVertexToState v =
        --     let t = v `mod` numTools
        --         t' = v `div` numTools in
        --     let x = t' `mod` width
        --         x' = t' `div` width in
        --     let y = x' in
        --     State { position: Pos { x, y }, tool: unsafePartial (fromJust (toEnum t)) }

        -- stateToVertex :: State -> Vertex
        -- stateToVertex (State { position: Pos p, tool }) = fromEnum tool + p.x * numTools + p.y * numTools * width

        startState = State { position: Pos { x: 0, y: 0 }, tool: Torch }
        -- startVertex = stateToVertex (State { position: Pos { x: 0, y: 0 }, tool: Torch })
        targetState = State { position: targetPos, tool: Torch }
        -- targetVertex = stateToVertex (State { position: targetPos, tool: Torch })

        -- adjacentVertices :: Vertex -> Array { vertex :: Vertex, cost :: Int }
        -- adjacentVertices v =
        --     let state = vertexToState v in
        --     unsafePartial $ validActions cave state <#> \a -> { vertex: stateToVertex (applyAction a state), cost: costOfAction a }

        adjacentStates :: State -> Array { vertex :: State, cost :: Int }
        adjacentStates state = unsafePartial $ validActions cave state <#> \a -> { vertex: applyAction a state, cost: costOfAction a }

        -- lowerBounds :: HashMap Int
        -- lowerBounds = calcLowerBound <$> Array.range 0 (numVertices - 1)

        lowerBoundFn v = calcLowerBound v -- unsafePartial $ Array.unsafeIndex lowerBounds v

        calcLowerBound :: State -> Int
        calcLowerBound (State { position: Pos p1, tool: t1 }) =
            let (Pos p2) = targetPos in
            abs (p1.x - p2.x) + abs (p1.y - p2.y) + (if t1 == Torch then 0 else 7)
