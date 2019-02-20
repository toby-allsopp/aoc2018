module Graph (
    kind GraphId,
    Graph,
    NodeId,
    graphFromEdges,
    nodeAt,
    edgesFrom,
    colourWithNumberOfIncomingEdges,
    nodesMatchingColourBy,
    modifyColourOfNode,
    mapColour
) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (fromJust)
import Data.Tuple (fst, snd)
import Partial.Unsafe (unsafePartial)

type Edge = { from :: Int, to :: Int }

foreign import kind GraphId

data Graph (id :: GraphId) node colour = Graph {
    nodes :: Array node,
    edges :: Array Edge,
    colours :: Array colour
}

data NodeId (id :: GraphId) = NodeId Int

instance nodeIdEq :: Eq (NodeId id) where
    eq (NodeId i1) (NodeId i2) = eq i1 i2

graphFromEdges :: forall id node. Ord node => Array { fromNode :: node, toNode :: node } -> Graph id node Unit
graphFromEdges nodeEdges = Graph { nodes, edges, colours }
    where
        nodes = (nodeEdges <#> (_.fromNode)) <> (nodeEdges <#> (_.toNode)) # Array.nub
        edges = nodeEdges <#> (\{fromNode, toNode} ->
            -- We KNOW that the steps are contained in nodes because we just put them there
            let from = unsafePartial $ fromJust $ Array.elemIndex fromNode nodes in
            let to   = unsafePartial $ fromJust $ Array.elemIndex toNode nodes in
            {from, to})
        colours = Array.replicate (Array.length nodes) unit

nodeAt :: forall id node colour. Graph id node colour -> NodeId id -> node
nodeAt (Graph { nodes }) (NodeId i) = unsafePartial $ Array.unsafeIndex nodes i

edgesFrom :: forall id node colour. Graph id node colour -> NodeId id -> Array (NodeId id)
edgesFrom (Graph { edges }) (NodeId i) = edges # Array.filter (\e -> e.from == i) <#> (\e -> NodeId e.to)

colourWithNumberOfIncomingEdges :: forall id node a. Graph id node a -> Graph id node Int
colourWithNumberOfIncomingEdges (Graph { nodes, edges }) =
    Graph { nodes, edges, colours: foldl f (Array.replicate (Array.length nodes) 0) edges }
    where
        f :: Array Int -> Edge -> Array Int
        f colours {from, to} = unsafePartial $ fromJust $ Array.modifyAt to (_+1) colours

nodesMatchingColourBy :: forall id node colour. (colour -> Boolean) -> Graph id node colour -> Array (NodeId id)
nodesMatchingColourBy pred (Graph { nodes, edges, colours }) = Array.filter (snd >>> pred) (Array.zip (Array.range 0 ((Array.length nodes) - 1)) colours) <#> fst <#> NodeId

modifyColourOfNode :: forall id node colour. NodeId id -> (colour -> colour) -> Graph id node colour -> Graph id node colour
modifyColourOfNode (NodeId nodeIndex) f (Graph { nodes, edges, colours }) =
    Graph {
        nodes,
        edges,
        colours : unsafePartial $ fromJust $ Array.modifyAt nodeIndex f colours
    }

mapColour :: forall id node colour1 colour2. (colour1 -> colour2) -> Graph id node colour1 -> Graph id node colour2
mapColour f (Graph { nodes, edges, colours }) = Graph {nodes, edges, colours : f <$> colours}