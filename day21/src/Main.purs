module Main where

import Prelude

import Data.Either (Either(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Console (log, logShow)

import Day21 as Day21
import Compiler as Compiler

input :: String
input = """#ip 1
seti 123 0 5
bani 5 456 5
eqri 5 72 5
addr 5 1 1
seti 0 0 1
seti 0 9 5
bori 5 65536 2
seti 7571367 9 5
bani 2 255 4
addr 5 4 5
bani 5 16777215 5
muli 5 65899 5
bani 5 16777215 5
gtir 256 2 4
addr 4 1 1
addi 1 1 1
seti 27 1 1
seti 0 2 4
addi 4 1 3
muli 3 256 3
gtrr 3 2 3
addr 3 1 1
addi 1 1 1
seti 25 6 1
addi 4 1 4
seti 17 8 1
setr 4 6 2
seti 7 4 1
eqrr 5 0 4
addr 4 1 1
seti 5 5 1
"""

main :: Effect Unit
main = do
    case Day21.parseInput input of
      Left error -> log error
      Right { ipRegister, program } -> do
        log `traverse_` (Compiler.compile ipRegister program)
        -- case Day19.assembleProgram program of
        --   Left error -> log error
        --   Right exe -> do
        --     let registers = Day19.execProgram ipRegister exe (Day19.intToRegisterValue <$> [0,0,0,0,0,0])
        --     logShow registers
        --     let part2registers = Day19.execProgram ipRegister exe (Day19.intToRegisterValue <$> [1,0,0,0,0,0])
        --     logShow part2registers
