module Day7 where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromJust)
import Data.String.Yarn (lines)
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Parser (char, eof, literal, runParser)
import Partial.Unsafe (unsafePartial)

type Step = Char
type Instruction = { beforeStep :: Step, afterStep :: Step }

type Edge = { from :: Int, to :: Int }

foreign import kind GraphId

data Graph (id :: GraphId) node colour = Graph {
    nodes :: Array node,
    edges :: Array Edge,
    colours :: Array colour
}

data NodeId (id :: GraphId) = NodeId Int

nodeAt :: forall id node colour. Graph id node colour -> NodeId id -> node
nodeAt (Graph { nodes }) (NodeId i) = unsafePartial $ Array.unsafeIndex nodes i

edgesFrom :: forall id node colour. Graph id node colour -> NodeId id -> Array (NodeId id)
edgesFrom (Graph { edges }) (NodeId i) = edges # Array.filter (\e -> e.from == i) <#> (\e -> NodeId e.to)

parseLine :: String -> Either String Instruction
parseLine = runParser do
    _ <- literal "Step "
    beforeStep <- char
    _ <- literal " must be finished before step "
    afterStep <- char
    _ <- literal " can begin."
    eof
    pure {beforeStep, afterStep}

parseInput :: String -> Either String (Array Instruction)
parseInput input = lines input <#> parseLine # sequence

graphOfInstructions :: forall id. Array Instruction -> Graph id Step Unit
graphOfInstructions instructions = Graph { nodes, edges, colours }
    where
        nodes = (instructions <#> (_.beforeStep)) <> (instructions <#> (_.afterStep)) # Array.nub
        edges = instructions <#> (\{beforeStep, afterStep} ->
            -- We KNOW that the steps are contained in nodes because we just put them there
            let from = unsafePartial $ fromJust $ Array.elemIndex beforeStep nodes in
            let to   = unsafePartial $ fromJust $ Array.elemIndex afterStep nodes in
            {from, to})
        colours = Array.replicate (Array.length nodes) unit

colourWithNumberOfIncomingEdges :: forall id node a. Graph id node a -> Graph id node Int
colourWithNumberOfIncomingEdges (Graph { nodes, edges }) =
    Graph { nodes, edges, colours: foldl f (Array.replicate (Array.length nodes) 0) edges }
    where
        f :: Array Int -> Edge -> Array Int
        f colours {from, to} = unsafePartial $ fromJust $ Array.modifyAt to (_+1) colours

nodesMatchingColourBy :: forall id node colour. (colour -> Boolean) -> Graph id node colour -> Array (NodeId id)
nodesMatchingColourBy pred (Graph { nodes, edges, colours }) = Array.filter (snd >>> pred) (Array.zip (Array.range 0 ((Array.length nodes) - 1)) colours) <#> fst <#> NodeId

readySteps :: forall id. Graph id Step Int -> Array (NodeId id)
readySteps = nodesMatchingColourBy (_ == 0)

modifyColourOfNode :: forall id node colour. NodeId id -> (colour -> colour) -> Graph id node colour -> Graph id node colour
modifyColourOfNode (NodeId nodeIndex) f (Graph { nodes, edges, colours }) =
    Graph {
        nodes,
        edges,
        colours : unsafePartial $ fromJust $ Array.modifyAt nodeIndex f colours
    }

markStepAsDone :: forall id. NodeId id -> Graph id Step Int -> Graph id Step Int
markStepAsDone step graph = foldl (\g s -> modifyColourOfNode s (_ - 1) g) graph (Array.snoc (edgesFrom graph step) step)

stepsInOrder :: Array Instruction -> Array Step
stepsInOrder instructions = go (colourWithNumberOfIncomingEdges (graphOfInstructions instructions)) []
    where
        go :: forall id. Graph id Step Int -> Array Step -> Array Step
        go graph steps =
            let nextSteps = readySteps graph # Array.sortWith (nodeAt graph) in
            case Array.head nextSteps of
                Nothing -> steps
                Just nextStep -> go (markStepAsDone nextStep graph) (Array.snoc steps (nodeAt graph nextStep))