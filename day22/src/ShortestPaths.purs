module ShortestPaths where

import Debug
import Prelude

import BinaryHeap (BinaryHeap)
import BinaryHeap as BH

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Foldable (elem, for_)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.HashMap as MashMap
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (for)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)

type Label v = { distance :: Int, next :: HashSet v }

type Labels h v = STRef h (HashMap v (Label v))

getLabel :: forall h r v. Hashable v => v -> Labels h v -> ST h (Maybe (Label v))
getLabel v labels = STRef.read labels <#> MashMap.lookup v

setLabel :: forall h r v. Hashable v => v -> Label v -> Labels h v -> ST h Unit
setLabel v label labels = STRef.read labels <#> HashMap.insert v label >>= flip STRef.write labels # void

type Adjacent v = { vertex :: v, cost :: Int }

type State h v = {
    labels :: Labels h v,
    adj :: v -> Array (Adjacent v),
    lowerBoundFn :: v -> Int,
    from :: v,
    tos :: HashSet v,
    upperBoundVar :: STRef h (Maybe Int)
}

updatedLabel :: forall v. Hashable v => v -> Int -> Maybe (Label v) -> Maybe (Label v)
updatedLabel from distance = case _ of
    Just l | l.distance < distance  -> Nothing
    Just l | l.distance == distance -> Just $ l { next = HashSet.insert from l.next }
    Just l | otherwise              ->
        -- debug ("relabel " <> show from <> ": " <> show l <> " -> " <> show distance) \_ ->
        Just { distance, next: HashSet.singleton from }
    Nothing                         -> Just { distance, next: HashSet.singleton from }

updateLabel :: forall h v. Hashable v => Partial => State h v -> Int -> v -> ST h Boolean
updateLabel {lowerBoundFn, from, tos, labels, upperBoundVar} distance pos = do
    upperBoundM <- STRef.read upperBoundVar
    case upperBoundM of
        Just upperBound | distance + lowerBoundFn from > upperBound -> pure false
        _ -> do
            oldLabel <- getLabel pos labels
            case updatedLabel pos distance oldLabel of
                newLabel@(Just nl) -> do
                    -- debug ("updateLabel " <> show pos <> ": " <> show oldLabel <> " -> " <> show newLabel) $ \_ -> do
                        setLabel pos nl labels
                        if HashSet.member pos tos
                            then debug ("update upper bound: " <> show distance) \_ -> void $ STRef.write (Just distance) upperBoundVar
                            else pure unit
                        -- when (HashSet.member pos tos) $ 
                        pure $ (Just nl.distance) /= (oldLabel <#> _.distance)
                Nothing -> pure false

labelFrom :: forall h v. Hashable v => Partial => State h v -> Int -> Array (Adjacent v) -> ST h (Array { vertex :: v, cost :: Int, lowerBound :: Int })
labelFrom state@{lowerBoundFn, from, tos, labels, upperBoundVar } distance adjacents =
    -- debug ("labelFrom " <> show distance <> " " <> show from <> ": " <> show adjacents) $ \_ ->
    do
        changed <- STArray.empty
        ST.foreach adjacents \{vertex, cost} -> do
            c <- updateLabel state (distance + cost) vertex
            when c $ void $ STArray.push {vertex, cost: distance + cost, lowerBound: lowerBoundFn vertex} changed
        STArray.unsafeFreeze changed

wibble :: forall r v. Hashable v => Show v => Partial => State r v -> v -> ST r (Array { vertex :: v, cost :: Int, lowerBound :: Int })
wibble state@{adj, lowerBoundFn, labels, upperBoundVar, tos} from = do
    let adjacents = adj from
    label <- getLabel from labels
    case label of
        Just l ->
            labelFrom state (fromJust label # _.distance) adjacents
        Nothing -> unsafeCrashWith $ "unlabelled vertex " <> show from

type Todo r v = BinaryHeap r { vertex :: v, cost :: Int, lowerBound :: Int }

minimumWithIndexBy :: forall f i a b. FoldableWithIndex i f => Ord b => (a -> b) -> f a -> Maybe { minIndex :: i, min :: a }
minimumWithIndexBy p = foldlWithIndex f Nothing
    where
        f i Nothing x = Just { minIndex: i, min: x }
        f i r@(Just {minIndex, min}) x = if p x < p min then Just { minIndex: i, min: x} else r

wobble :: forall r v. Hashable v => Show v => Partial => State r v -> Todo r v -> ST r Unit
wobble state@{adj, lowerBoundFn, tos, labels, upperBoundVar} todo = tailRecM go unit
    where
    go :: Unit -> ST r (Step Unit Unit)
    go _ = do
        first <- BH.extractMin todo
        -- debug ("wobble " <> show first) $ \_ ->
        case first of
            Nothing -> pure $ Done unit
            Just from -> do
                adjacents <- wibble state from.vertex
                for_ adjacents \a -> do
                    BH.insert a todo
                pure $ Loop unit

fribble :: forall r v. Hashable v => Show v => (v -> Array (Adjacent v)) -> (v -> Int) -> v -> HashSet v -> ST r (HashMap v (Label v))
fribble adj lowerBoundFn from tos = do
    labels <- STRef.new (HashMap.empty)
    upperBoundVar <- STRef.new Nothing
    setLabel from { distance: 0, next: HashSet.empty } labels
    todo <- BH.empty (comparing (\t -> t.cost + t.lowerBound))
    BH.insert {vertex: from, cost: 0, lowerBound: lowerBoundFn from} todo
    unsafePartial $ wobble {adj, lowerBoundFn, from, tos, labels, upperBoundVar} todo
    STRef.read labels

labelShortestPathsTo :: forall v. Hashable v => Show v => (v -> Array (Adjacent v)) -> (v -> Int) -> v -> Array v -> HashMap v (Label v)
labelShortestPathsTo adj lowerBoundFn from tos = ST.run (fribble adj lowerBoundFn from (HashSet.fromFoldable tos))

-- followPaths :: Partial => Array (Maybe Label) -> Vertex -> Array (Array Vertex)
-- followPaths sps = followPaths' []
--     where
--     followPaths' :: Partial => Array Vertex -> Vertex -> Array (Array Vertex)
--     followPaths' seen p =
--         if p `elem` seen then
--             unsafeCrashWith $ "cycle detected! " <> show (Array.snoc seen p)
--         else
--             let maybeLabel = fromJust $ Array.index sps p in
--             let label = fromJust maybeLabel in
--             if HashSet.size label.next == 0 then [[p]] else
--                 let paths = followPaths' (Array.snoc seen p) =<< HashSet.toArray label.next in
--                 Array.cons p <$> paths

-- labelToNextLabels :: Array (Maybe Label) -> Label -> Array Label
-- labelToNextLabels sps { next } = Array.catMaybes $ join <$> Array.index sps <$> HashSet.toArray next

-- followNextsUntilDistance :: Int -> Array (Maybe Label) -> Vertex -> Array Vertex
-- followNextsUntilDistance d sps from =
--     case Array.index sps from of
--         Nothing -> []
--         Just Nothing -> []
--         Just (Just label) -> go [from] label.distance
--     where
--     go froms distance =
--         -- debug ("followNextsUntilDistance " <> show d <> " " <> show distance <> " " <> show froms) $ \_ ->
--         if distance <= d then froms
--         else
--             let labels = Array.catMaybes $ join <$> Array.index sps <$> froms in
--             let (nextVertexs :: Array Vertex) = HashSet.toArray $ HashSet.fromFoldable $ join $ HashSet.toArray <$> _.next <$> labels in
--             go nextVertexs (distance - 1)

-- type Path = { distance :: Int, paths :: Array (Array Vertex) }

-- shortestPathsFrom :: Vertex -> Array (Maybe Label) -> Maybe Path
-- shortestPathsFrom p sps = do
--     maybeLabel <- Array.index sps p
--     label <- maybeLabel
--     let paths = unsafePartial $ followPaths sps p
--     pure { distance: label.distance, paths }
