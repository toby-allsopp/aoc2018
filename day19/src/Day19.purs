module Day19 where

import Prelude

import Control.Alt ((<|>))
import Data.Array (foldl, foldr)
import Data.Array as Array
import Data.Either (Either)
import Data.Enum as Enum
import Data.Function (on)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Hashable (class Hashable, hash)
import Data.Int.Bits (and, or)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, un)
import Data.Tuple (Tuple(..))
import Debug
import Parser as P
import Partial.Unsafe (unsafeCrashWith)

type Registers = Array Int

regValue :: Registers -> Int -> Int
regValue r i = case Array.index r i of
    Just x -> x
    Nothing -> unsafeCrashWith $ "Invalid register number " <> show i

newtype OpcodeDescription = OpcodeDescription {
    name :: String,
    behaviour :: Registers -> { a :: Int, b :: Int } -> Int
}

derive instance newtypeOpcodeDescription :: Newtype OpcodeDescription _

instance showOpcodeDescription :: Show OpcodeDescription where
    show = show <<< opcodeDescriptionName

instance eqOpcodeDescription :: Eq OpcodeDescription where
    eq = eq `on` opcodeDescriptionName

instance hashableOpcodeDescription :: Hashable OpcodeDescription where
    hash (OpcodeDescription { name }) = hash name

mkOpcodeRR :: String -> (Int -> Int -> Int) -> OpcodeDescription
mkOpcodeRR name f = OpcodeDescription { name, behaviour: \r {a, b} -> f (r `regValue` a) (r `regValue` b) }

mkOpcodeRI :: String -> (Int -> Int -> Int) -> OpcodeDescription
mkOpcodeRI name f = OpcodeDescription { name, behaviour: \r {a, b} -> f (r `regValue` a) b }

mkOpcodeIR :: String -> (Int -> Int -> Int) -> OpcodeDescription
mkOpcodeIR name f = OpcodeDescription { name, behaviour: \r {a, b} -> f a (r `regValue` b) }

mkOpcodeIX :: String -> (Int -> Int) -> OpcodeDescription
mkOpcodeIX name f = OpcodeDescription { name, behaviour: \r {a, b} -> f a }

boolValue :: (Int -> Int -> Boolean) -> Int -> Int -> Int
boolValue f x y = Enum.fromEnum (f x y)

opcodes :: HashMap String OpcodeDescription
opcodes = HashMap.fromArrayBy opcodeDescriptionName identity [
    mkOpcodeRR "addr" (+),
    mkOpcodeRI "addi" (+),
    mkOpcodeRR "mulr" (*),
    mkOpcodeRI "muli" (*),
    mkOpcodeRR "banr" and,
    mkOpcodeRI "bani" and,
    mkOpcodeRR "borr" or,
    mkOpcodeRI "bori" or,
    mkOpcodeRR "setr" const,
    mkOpcodeIX "seti" identity,
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

type Instruction = { opcode :: String, args :: Args }

opcodeDescriptionName :: OpcodeDescription -> String
opcodeDescriptionName = un OpcodeDescription >>> (_.name)

execProgram :: Int -> Array Instruction -> Registers
execProgram ipRegister instructions = go (Array.replicate 6 0)
    where
    go :: Registers -> Registers
    go r =
        case r Array.!! ipRegister of
            Nothing -> r
            Just ip ->
                case instructions Array.!! ip of
                    Nothing -> r
                    Just i ->
                        case HashMap.lookup i.opcode opcodes of
                            Nothing -> r
                            Just opcode ->
                                let r' = execOpcode opcode r i.args in
                                case Array.modifyAt ipRegister (_ + 1) r' of
                                    Nothing -> r
                                    Just r'' ->
                                        go r''

type Parser = P.Parser Char

parseInstruction :: Parser Instruction
parseInstruction = do
    opcode <- P.word
    _ <- P.literal " "
    a <- P.integer
    _ <- P.literal " "
    b <- P.integer
    _ <- P.literal " "
    c <- P.integer
    pure { opcode, args: {a, b, c} }

parseInput :: String -> Either String { ipRegister :: Int, program :: Array Instruction }
parseInput = P.runParser do
    _ <- P.literal "#ip "
    ipRegister <- P.integer
    P.eol
    program <- P.separatedList (P.literal "\n") parseInstruction
    pure { ipRegister, program }