module Main where

import Prelude

import Day16 as Day16
import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Either (Either(..))
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input.txt"
  case Day16.parseInput input of
    Left error -> log error
    Right { samples, program } -> do
      logShow $ Day16.numberThatBehaveLikeThreeOrMore samples
