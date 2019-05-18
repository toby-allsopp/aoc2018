module Day16 where

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

type OpcodeNumberToPossibleOpcodes = HashMap Int (Array OpcodeDescription)

opcodeDescriptionName :: OpcodeDescription -> String
opcodeDescriptionName = un OpcodeDescription >>> (_.name)

initialPossibilities :: Array OpcodeDescription
initialPossibilities = opcodes

evaluateSample :: Sample -> OpcodeNumberToPossibleOpcodes -> OpcodeNumberToPossibleOpcodes
evaluateSample sample =
    HashMap.alter
        (Just <<<
         Array.intersect (opcodesSampleBehavesLike sample) <<<
         fromMaybe initialPossibilities)
        sample.instruction.opcode

evaluateSamples :: Array Sample -> OpcodeNumberToPossibleOpcodes
evaluateSamples = foldr evaluateSample HashMap.empty

removeUnambiguous :: OpcodeNumberToPossibleOpcodes -> { known :: HashMap OpcodeDescription Int, unknown :: OpcodeNumberToPossibleOpcodes }
removeUnambiguous possibilities =
    let justUnambiguous opcodeNumber possibleOpcodes =
            case possibleOpcodes of
                [opcode] -> Just opcode
                _ -> Nothing in
    let known = HashMap.mapMaybeWithKey justUnambiguous possibilities
            # HashMap.toArrayBy (flip Tuple) # HashMap.fromArray in
    let justNonEmpty = case _ of
            [] -> Nothing
            a -> Just a in
    let unknown = HashMap.mapMaybe (justNonEmpty <<< Array.filter (\opcode -> not $ HashMap.member opcode known)) possibilities in
    { known, unknown }

solveSamples :: Array Sample -> HashMap Int OpcodeDescription
solveSamples samples = solve' HashMap.empty (evaluateSamples samples)
    where
    solve' known unknown =
        if HashMap.isEmpty unknown then
            HashMap.fromArray $ HashMap.toArrayBy (flip Tuple) known
        else
            let { known: newKnown, unknown } = removeUnambiguous unknown in
            if HashMap.isEmpty newKnown then unsafeCrashWith "unable to solve" else
                solve' (HashMap.union known newKnown) unknown

execProgram :: HashMap Int OpcodeDescription -> Array Instruction -> Registers
execProgram opcodeMapping instructions = foldl go (Array.replicate 4 0) instructions
    where
    go :: Registers -> Instruction -> Registers
    go r i =
        case HashMap.lookup i.opcode opcodeMapping of
            Just opcode -> execOpcode opcode r i.args
            Nothing -> unsafeCrashWith $ "unmapped opcode " <> show i.opcode

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