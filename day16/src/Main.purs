module Main where

import Prelude

import Data.Either (Either(..))
import Day16 as Day16
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input.txt"
  case Day16.parseInput input of
    Left error -> log error
    Right { samples, program } -> do
      logShow $ Day16.numberThatBehaveLikeThreeOrMore samples
      let unknown = Day16.evaluateSamples samples
      logShow $ unknown
      let { known, unknown } = Day16.removeUnambiguous unknown
      logShow $ known
      logShow $ unknown
      let opcodeMapping = Day16.solveSamples samples
      logShow $ opcodeMapping
      let registers = Day16.execProgram opcodeMapping program
      logShow registers
