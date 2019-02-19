module Day7 where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String.Yarn (lines)
import Data.Traversable (sequence)
import Graph (Graph, NodeId)
import Graph as Graph
import Parser (char, eof, literal, runParser)

type Step = Char
type Instruction = { beforeStep :: Step, afterStep :: Step }

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
graphOfInstructions instructions = Graph.graphFromEdges $ instructions <#> \{beforeStep, afterStep} -> {fromNode: beforeStep, toNode: afterStep}

readySteps :: forall id. Graph id Step Int -> Array (NodeId id)
readySteps = Graph.nodesMatchingColourBy (_ == 0)

markStepAsDone :: forall id. NodeId id -> Graph id Step Int -> Graph id Step Int
markStepAsDone step graph = foldl (\g s -> Graph.modifyColourOfNode s (_ - 1) g) graph (Array.snoc (Graph.edgesFrom graph step) step)

stepsInOrder :: Array Instruction -> Array Step
stepsInOrder instructions = go (Graph.colourWithNumberOfIncomingEdges (graphOfInstructions instructions)) []
    where
        go :: forall id. Graph id Step Int -> Array Step -> Array Step
        go graph steps =
            let nextSteps = readySteps graph # Array.sortWith (Graph.nodeAt graph) in
            case Array.head nextSteps of
                Nothing -> steps
                Just nextStep -> go (markStepAsDone nextStep graph) (Array.snoc steps (Graph.nodeAt graph nextStep))
