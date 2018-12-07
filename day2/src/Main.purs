module Main where

import Prelude

import Day2 as Day2
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input.txt"
  let ids = Day2.parseInput input
  log $ show $ Day2.checksum ids
  log $ show $ Day2.lettersInCommonInCorrectBoxIds ids
