module Day7 (Step, Instruction, parseInput, stepsInOrderWithWorkers, part2NodeDuration) where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Enum (fromEnum)
import Data.Foldable (foldl)
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

data State
    = Waiting { incomingEdges :: Int }
    | InProgress { remainingDuration :: Int }
    | Done

isReady :: State -> Boolean
isReady (Waiting { incomingEdges : 0 }) = true
isReady _ = false

isInProgress :: State -> Boolean
isInProgress (InProgress _) = true
isInProgress _ = false

isFinished :: State -> Boolean
isFinished (InProgress { remainingDuration : 0 }) = true
isFinished _ = false

readySteps :: forall id. Graph id Step State -> Array (NodeId id)
readySteps = Graph.nodesMatchingColourBy isReady

countInProgressSteps :: forall id. Graph id Step State -> Int
countInProgressSteps = Graph.nodesMatchingColourBy isInProgress >>> Array.length

markStepAsInProgress :: forall id. Int -> NodeId id -> Graph id Step State -> Graph id Step State
markStepAsInProgress duration step graph = Graph.modifyColourOfNode step (const (InProgress { remainingDuration : duration })) graph

markStepsAsInProgress :: forall id. (Step -> Int) -> Array (NodeId id) -> Graph id Step State -> Graph id Step State
markStepsAsInProgress f steps graph = foldl (\g s -> markStepAsInProgress (f (Graph.nodeAt graph s)) s g) graph steps

finishedSteps :: forall id. Graph id Step State -> Array (NodeId id)
finishedSteps = Graph.nodesMatchingColourBy isFinished

decIncomingEdges :: State -> State
decIncomingEdges (Waiting { incomingEdges }) = Waiting { incomingEdges : incomingEdges - 1 }
decIncomingEdges s = s

markStepAsDone :: forall id. NodeId id -> Graph id Step State -> Graph id Step State
markStepAsDone step graph =
    let graph' = Graph.modifyColourOfNode step (const Done) graph in
    foldl (\g s -> Graph.modifyColourOfNode s decIncomingEdges g) graph' (Graph.edgesFrom graph step)

markFinishedStepsAsDone :: forall id. Array (NodeId id) -> Graph id Step State -> Graph id Step State
markFinishedStepsAsDone finished graph =
    foldl (\g s -> markStepAsDone s g) graph finished

decRemainingDuration :: State -> State
decRemainingDuration (InProgress { remainingDuration }) = InProgress { remainingDuration : remainingDuration - 1 }
decRemainingDuration s = s

decAllRemainingDurations :: forall id. Graph id Step State -> Graph id Step State
decAllRemainingDurations = Graph.mapColour decRemainingDuration

stepsInOrderWithWorkers :: Int -> (Step -> Int) -> Array Instruction -> { t :: Int, steps :: Array Step }
stepsInOrderWithWorkers workers nodeDuration instructions =
    go (instructions #
        graphOfInstructions #
        Graph.colourWithNumberOfIncomingEdges #
        Graph.mapColour (\incomingEdges -> Waiting { incomingEdges })) { t : 0, steps : [] }
    where
        go :: forall id. Graph id Step State -> { t :: Int, steps :: Array Step } -> { t :: Int, steps :: Array Step }
        go graph { t, steps } =
            let finished = finishedSteps graph in
            let finishedGraph = markFinishedStepsAsDone finished graph in
            let numInProgress = countInProgressSteps finishedGraph in
            let numFreeWorkers = workers - numInProgress in
            let ready = readySteps finishedGraph # Array.sortWith (Graph.nodeAt finishedGraph) # Array.take numFreeWorkers in
            let steps' = steps <> (Graph.nodeAt finishedGraph <$> finished) in
            if Array.null ready && numInProgress == 0 then
                { t, steps : steps' }
            else
                go (finishedGraph # markStepsAsInProgress nodeDuration ready # decAllRemainingDurations) { t : (t+1), steps : steps' }

part2NodeDuration :: Step -> Int
part2NodeDuration = fromEnum >>> (_ - fromEnum 'A') >>> (_ + 61)        