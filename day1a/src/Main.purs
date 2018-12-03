module Main where

import Prelude

import Day1 (sumFrequencyChanges, firstRepeatedFrequencyChange)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input.txt"
  log $ show $ sumFrequencyChanges input
  log $ show $ firstRepeatedFrequencyChange input
