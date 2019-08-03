module Main where

import Prelude

import Data.Either (Either(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Console (log, logShow)

import Day19 as Day19
import Compiler as Compiler

input :: String
input = """#ip 3
addi 3 16 3
seti 1 8 5
seti 1 0 4
mulr 5 4 2
eqrr 2 1 2
addr 2 3 3
addi 3 1 3
addr 5 0 0
addi 4 1 4
gtrr 4 1 2
addr 3 2 3
seti 2 3 3
addi 5 1 5
gtrr 5 1 2
addr 2 3 3
seti 1 4 3
mulr 3 3 3
addi 1 2 1
mulr 1 1 1
mulr 3 1 1
muli 1 11 1
addi 2 4 2
mulr 2 3 2
addi 2 19 2
addr 1 2 1
addr 3 0 3
seti 0 7 3
setr 3 2 2
mulr 2 3 2
addr 3 2 2
mulr 3 2 2
muli 2 14 2
mulr 2 3 2
addr 1 2 1
seti 0 1 0
seti 0 5 3
"""

main :: Effect Unit
main = do
    case Day19.parseInput input of
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
