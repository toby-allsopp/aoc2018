module Day20 where

import Prelude

import Data.Foldable (foldl, maximum)
import Data.Maybe (fromMaybe)
import Regex (Dir(..), Regex(..))

import Data.Array as Array
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Tuple (Tuple(..))

type Pos = { x :: Int, y :: Int }

type Label = { shortestDistance :: Int }

type Labels = HashMap Pos Label

move :: Dir -> Pos -> Pos
move N {x, y} = {x, y: y + 1}
move S {x, y} = {x, y: y - 1}
move E {x, y} = {x: x + 1, y}
move W {x, y} = {x: x - 1, y}

updateLabel :: Pos -> Int -> Labels -> Labels
updateLabel p dist = HashMap.insertWith update p { shortestDistance: dist }
    where
    update existing new = { shortestDistance: min existing.shortestDistance new.shortestDistance }

type State = { pos :: Pos, dist :: Int, labels :: Labels }

followRegex :: Regex -> Labels
followRegex = follow { pos: {x:0, y:0}, dist: 0, labels: HashMap.fromArray [Tuple {x:0, y:0} {shortestDistance:0}] } >>> _.labels
    where
        follow :: State -> Regex -> State
        follow state (Atom d) = atom state d
        follow state (Sequence rs) = foldl follow state rs
        follow state (Branch rs) = foldl (branch state) state rs

        atom :: State -> Dir -> State
        atom {pos, dist, labels} d =
            let nextPos = move d pos in
            {pos: nextPos, dist: dist + 1, labels: updateLabel nextPos (dist + 1) labels}

        branch :: State -> State -> Regex -> State
        branch origState state r = follow (origState { labels = state.labels }) r

furthest :: Labels -> Int
furthest = HashMap.values >>> map _.shortestDistance >>> maximum >>> fromMaybe 0

countShortestPathsSuchThat :: (Int -> Boolean) -> Labels -> Int
countShortestPathsSuchThat p =
    HashMap.values >>> Array.filter (_.shortestDistance >>> p) >>> Array.length