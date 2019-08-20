module Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Regex as Regex
import Day20 as Day20

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input.txt"
  case Regex.parse input of
    Left err -> log err
    Right regex -> do
      log "parsed"
      let labels = Day20.followRegex regex
      log "followed"
      logShow $ Day20.furthest labels
      logShow $ Day20.countShortestPathsSuchThat (\d -> d >= 1000) labels
