module Day21 where

import Debug
import Prelude

import Control.Alt ((<|>))
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (ST, kind Region)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Data.Array (foldl, foldr)
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Either (Either(..))
import Data.Enum as Enum
import Data.Function (on)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Hashable (class Hashable, hash)
import Data.Int (floor, toNumber)
import Data.Int.Bits (and, or)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, un)
import Data.Traversable (traverse)
import Data.Traversable as Math
import Data.Tuple (Tuple(..))
import Parser as P
import Partial.Unsafe (unsafeCrashWith)

type RegisterValue = Number

intToRegisterValue :: Int -> RegisterValue
intToRegisterValue = toNumber

registerValueToInt :: RegisterValue -> Int
registerValueToInt = floor

type Registers = Array RegisterValue
type STRegisters r = STArray r RegisterValue

regValue :: Registers -> Int -> RegisterValue
regValue r i = case Array.index r i of
    Just x -> x
    Nothing -> unsafeCrashWith $ "Invalid register number " <> show i

newtype OpcodeDescription = OpcodeDescription {
    name :: String,
    behaviour :: Registers -> { a :: Int, b :: Int } -> RegisterValue
}

derive instance newtypeOpcodeDescription :: Newtype OpcodeDescription _

instance showOpcodeDescription :: Show OpcodeDescription where
    show = show <<< opcodeDescriptionName

instance eqOpcodeDescription :: Eq OpcodeDescription where
    eq = eq `on` opcodeDescriptionName

instance hashableOpcodeDescription :: Hashable OpcodeDescription where
    hash (OpcodeDescription { name }) = hash name

mkOpcodeRR :: String -> (RegisterValue -> RegisterValue -> RegisterValue) -> OpcodeDescription
mkOpcodeRR name f = OpcodeDescription { name, behaviour: \r {a, b} -> f (r `regValue` a) (r `regValue` b) }

mkOpcodeRI :: String -> (RegisterValue -> RegisterValue -> RegisterValue) -> OpcodeDescription
mkOpcodeRI name f = OpcodeDescription { name, behaviour: \r {a, b} -> f (r `regValue` a) (intToRegisterValue b) }

mkOpcodeIR :: String -> (RegisterValue -> RegisterValue -> RegisterValue) -> OpcodeDescription
mkOpcodeIR name f = OpcodeDescription { name, behaviour: \r {a, b} -> f (intToRegisterValue a) (r `regValue` b) }

mkOpcodeIX :: String -> (RegisterValue -> RegisterValue) -> OpcodeDescription
mkOpcodeIX name f = OpcodeDescription { name, behaviour: \r {a, b} -> f (intToRegisterValue a) }

boolValue :: (RegisterValue -> RegisterValue -> Boolean) -> RegisterValue -> RegisterValue -> RegisterValue
boolValue f x y = intToRegisterValue (Enum.fromEnum (f x y))

intValue :: (Int -> Int -> Int) -> RegisterValue -> RegisterValue -> RegisterValue
intValue f x y = intToRegisterValue (f (registerValueToInt x) (registerValueToInt y))

opcodes :: HashMap String OpcodeDescription
opcodes = HashMap.fromArrayBy opcodeDescriptionName identity [
    mkOpcodeRR "addr" (+),
    mkOpcodeRI "addi" (+),
    mkOpcodeRR "mulr" (*),
    mkOpcodeRI "muli" (*),
    mkOpcodeRR "banr" (intValue and),
    mkOpcodeRI "bani" (intValue and),
    mkOpcodeRR "borr" (intValue or),
    mkOpcodeRI "bori" (intValue or),
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

execOpcodeST :: forall r. OpcodeDescription -> STRegisters r -> Args -> ST r Unit
execOpcodeST (OpcodeDescription { behaviour }) str {a, b, c} = do
    r <- STArray.unsafeFreeze str
    ok <- STArray.poke c (behaviour r {a, b}) str
    case ok of
        true -> pure unit
        false -> unsafeCrashWith $ "Invalid output register number " <> show c

type Instruction = { opcode :: String, args :: Args }
type ResolvedInstruction = { opcode :: OpcodeDescription, args :: Args }

opcodeDescriptionName :: OpcodeDescription -> String
opcodeDescriptionName = un OpcodeDescription >>> (_.name)

lookupInstruction :: Instruction -> Either String ResolvedInstruction
lookupInstruction i = case HashMap.lookup i.opcode opcodes of
    Nothing -> Left ("opcode '" <> i.opcode <> "' not valid")
    Just opcode -> Right { opcode, args: i.args }

assembleProgram :: Array Instruction -> Either String (Array ResolvedInstruction)
assembleProgram = traverse lookupInstruction

execProgram :: Int -> Array ResolvedInstruction -> Registers -> Registers
execProgram ipRegister instructions registers = ST.run (STArray.withArray go registers)
    where
    go :: forall r. STRegisters r -> ST r Number
    go r = do
        prevR0Var <- STRef.new =<< STArray.peek 0 r
        count <- tailRecM (go' prevR0Var) 0.0
        debug (show count) \_ -> pure count
        where
        go' :: STRef r (Maybe RegisterValue) -> Number -> ST r (Step Number Number)
        go' prevR0Var count = do
            prevR0 <- STRef.read prevR0Var
            thisR0 <- STArray.peek 0 r
            if prevR0 /= thisR0
                then debug (show count <> ": " <> show thisR0) \_ -> void $ STRef.write thisR0 prevR0Var
                else pure unit
            mip <- STArray.peek ipRegister r
            case mip of
                Nothing -> pure (Done count)
                Just ip ->
                    case instructions Array.!! (floor ip) of
                        Nothing -> pure (Done count)
                        Just i -> do
                            execOpcodeST i.opcode r i.args
                            ok <- STArray.modify ipRegister (_ + 1.0) r
                            case ok of
                                false -> pure (Done count)
                                true -> pure (Loop (count + 1.0))

type Parser = P.Parser String

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