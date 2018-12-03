module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Day2 as Day2

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input.txt"
  let ids = Day2.parseInput input
  log $ show $ Day2.checksum ids
