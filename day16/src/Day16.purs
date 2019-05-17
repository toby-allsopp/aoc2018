module Day16 where

import Prelude

import Parser as P

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either)
import Data.Enum as Enum
import Data.Int.Bits
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafeCrashWith)

type Registers = Array Int

regValue :: Registers -> Int -> Int
regValue r i = case Array.index r i of
    Just x -> x
    Nothing -> unsafeCrashWith $ "Invalid register number " <> show i

data OpcodeDescription = OpcodeDescription {
    name :: String,
    behaviour :: Registers -> { a :: Int, b :: Int } -> Int
}

mkOpcodeRR :: String -> (Int -> Int -> Int) -> OpcodeDescription
mkOpcodeRR name f = OpcodeDescription { name, behaviour: \r {a, b} -> f (r `regValue` a) (r `regValue` b) }

mkOpcodeRI :: String -> (Int -> Int -> Int) -> OpcodeDescription
mkOpcodeRI name f = OpcodeDescription { name, behaviour: \r {a, b} -> f (r `regValue` a) b }

mkOpcodeIR :: String -> (Int -> Int -> Int) -> OpcodeDescription
mkOpcodeIR name f = OpcodeDescription { name, behaviour: \r {a, b} -> f a (r `regValue` b) }

boolValue :: (Int -> Int -> Boolean) -> Int -> Int -> Int
boolValue f x y = Enum.fromEnum (f x y)

opcodes :: Array OpcodeDescription
opcodes = [
    mkOpcodeRR "addr" (+),
    mkOpcodeRI "addi" (+),
    mkOpcodeRR "mulr" (*),
    mkOpcodeRI "muli" (*),
    mkOpcodeRR "banr" and,
    mkOpcodeRI "bani" and,
    mkOpcodeRR "borr" or,
    mkOpcodeRI "bori" or,
    mkOpcodeRR "setr" const,
    mkOpcodeIR "seti" const,
    mkOpcodeIR "gtir" (boolValue (>)),
    mkOpcodeRI "gtri" (boolValue (>)),
    mkOpcodeRR "gtrr" (boolValue (>)),
    mkOpcodeIR "eqir" (boolValue (==)),
    mkOpcodeRI "eqri" (boolValue (==)),
    mkOpcodeRR "eqrr" (boolValue (==))
]

type Args = { a :: Int, b :: Int, c :: Int }

execOpcode :: OpcodeDescription -> Registers -> Args -> Registers
execOpcode (OpcodeDescription { behaviour }) r {a, b, c} =
    case Array.updateAt c (behaviour r {a, b}) r of
        Just result -> result
        Nothing -> unsafeCrashWith $ "Invalid output register number " <> show c

type Instruction = { opcode :: Int, args :: Args }

type Sample = { before :: Registers, instruction :: Instruction, after :: Registers }

sampleBehavesLikeOpcode :: Sample -> OpcodeDescription -> Boolean
sampleBehavesLikeOpcode sample opcode =
    execOpcode opcode sample.before sample.instruction.args == sample.after

opcodesSampleBehavesLike :: Sample -> Array OpcodeDescription
opcodesSampleBehavesLike sample = Array.filter (sampleBehavesLikeOpcode sample) opcodes

numberThatBehaveLikeThreeOrMore :: Array Sample -> Int
numberThatBehaveLikeThreeOrMore =
    Array.filter (opcodesSampleBehavesLike >>> Array.length >>> (_ >= 3))
    >>> Array.length

type Parser = P.Parser Char

parseSample :: Parser Sample
parseSample = do
    _ <- P.literal "Before: "
    before <- parseRegisters
    _ <- P.literal "\n"
    instruction <- parseInstruction
    _ <- P.literal "\n"
    _ <- P.literal "After:  "
    after <- parseRegisters
    _ <- P.literal "\n"
    pure { before, instruction, after }

separatedList :: forall a b. Parser b -> Parser a -> Parser (Array a)
separatedList sepParser elementParser = do
    head <- elementParser
    tail <- (do
        _ <- sepParser
        separatedList sepParser elementParser
    ) <|> pure []
    pure $ Array.cons head tail

parseRegisters :: Parser Registers
parseRegisters = do
    _ <- P.literal "["
    registers <- separatedList (P.literal ", ") P.integer
    _ <- P.literal "]"
    pure registers

parseInstruction :: Parser Instruction
parseInstruction = do
    opcode <- P.integer
    _ <- P.literal " "
    a <- P.integer
    _ <- P.literal " "
    b <- P.integer
    _ <- P.literal " "
    c <- P.integer
    pure { opcode, args: {a, b, c} }

parseInput :: String -> Either String { samples :: Array Sample, program :: Array Instruction }
parseInput = P.runParser do
    samples <- separatedList (P.literal "\n") parseSample
    _ <- P.literal "\n\n\n"
    program <- separatedList (P.literal "\n") parseInstruction
    pure { samples, program }