module Compiler where

import Control.Monad.State
import Data.Array
import Data.FunctorWithIndex
import Data.TraversableWithIndex
import Day19
import Prelude

import Data.Array as Array
import Data.Int.Bits (and, or)
import Data.Maybe (fromJust, fromMaybe)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

type TrackedRegisters = Array Value

type Tracker a = State TrackedRegisters a

compile :: Int -> Array Instruction -> Array String
compile ipRegister instructions =
    let instExprs = instructionsToExpressions instructions in
    flip evalState (0 .. 5 <#> Register) $
        forWithIndex instExprs \ip {expression, outputRegister} -> do
            unsafePartial $ modify_ $ fromJust <<< Array.updateAt ipRegister (Immediate (intToRegisterValue ip))
            line <- if outputRegister == ipRegister then compileJump ipRegister {expression, outputRegister} else compileNormal ipRegister {expression, outputRegister}
            pure $ "L" <> show ip <> ":\n" <> line

data Value
    = Register Int
    | Immediate RegisterValue

instance showValue :: Show Value where
    show (Register i) = "r[" <> show i <> "]"
    show (Immediate x) = show x

data Expression
    = Add Expression Expression
    | Mul Expression Expression
    | Ban Expression Expression
    | Bor Expression Expression
    | Set Value
    | Gt Expression Expression
    | Eq Expression Expression

instance expressionShow :: Show Expression where
    show (Add e1 e2) = "(" <> show e1 <> ") + (" <> show e2 <> ")"
    show (Mul e1 e2) = "(" <> show e1 <> ") * (" <> show e2 <> ")"
    show (Ban e1 e2) = "(" <> show e1 <> ") & (" <> show e2 <> ")"
    show (Bor e1 e2) = "(" <> show e1 <> ") | (" <> show e2 <> ")"
    show (Set v) = show v
    show (Gt e1 e2) = "(" <> show e1 <> ") > (" <> show e2 <> ")"
    show (Eq e1 e2) = "(" <> show e1 <> ") == (" <> show e2 <> ")"

type ExpressionInstruction = { expression :: Expression, outputRegister :: Int }

instructionsToExpressions :: Array Instruction -> Array ExpressionInstruction
instructionsToExpressions = map instructionToExpression

instructionToExpression :: Instruction -> ExpressionInstruction
instructionToExpression {opcode, args} =
    let ra = Register args.a
        ia = Immediate (intToRegisterValue args.a)
        rb = Register args.b
        ib = Immediate (intToRegisterValue args.b)
        outputRegister = args.c
    in
    let expression = case opcode of
            "addr" -> Add (Set ra) (Set rb)
            "addi" -> Add (Set ra) (Set ib)
            "mulr" -> Mul (Set ra) (Set rb)
            "muli" -> Mul (Set ra) (Set ib)
            "banr" -> Ban (Set ra) (Set rb)
            "bani" -> Ban (Set ra) (Set ib)
            "borr" -> Bor (Set ra) (Set rb)
            "bori" -> Bor (Set ra) (Set ib)
            "setr" -> Set ra
            "seti" -> Set ia
            "gtir" -> Gt (Set ia) (Set rb)
            "gtri" -> Gt (Set ra) (Set ib)
            "gtrr" -> Gt (Set ra) (Set rb)
            "eqir" -> Eq (Set ia) (Set rb)
            "eqri" -> Eq (Set ra) (Set ib)
            "eqrr" -> Eq (Set ra) (Set rb)
            _ -> unsafeCrashWith $ "Unknonwn opcode " <> opcode
    in { expression, outputRegister }

evalValue :: TrackedRegisters -> Value -> Value
evalValue r (Register i) = fromMaybe (Register i) (r !! i)
evalValue r (Immediate v) = Immediate v

foo :: TrackedRegisters
    -> (Expression -> Expression -> Expression)
    -> (RegisterValue -> RegisterValue -> RegisterValue)
    -> Expression -> Expression -> Expression
foo r c op e1 e2 = case (simplify r e1), (simplify r e2) of
    (Set (Immediate x)), (Set (Immediate y)) -> Set (Immediate (x `op` y))
    se1, se2 -> c se1 se2

simplify :: TrackedRegisters -> Expression -> Expression
simplify r (Add e1 e2) = foo r Add (+) e1 e2
simplify r (Mul e1 e2) = foo r Mul (*) e1 e2
simplify r (Ban e1 e2) = foo r Ban (intValue and) e1 e2
simplify r (Bor e1 e2) = foo r Bor (intValue or) e1 e2
simplify r (Set v) = Set (evalValue r v)
simplify r (Gt e1 e2) = foo r Gt (boolValue (>)) e1 e2
simplify r (Eq e1 e2) = foo r Eq (boolValue (==)) e1 e2

evalExpression :: Int -> Expression -> Value
evalExpression i (Set (Immediate x)) = Immediate x
evalExpression i _ = Register i

compileJump :: Int -> ExpressionInstruction -> Tracker String
compileJump ipRegister {expression, outputRegister} = do
    r <- get
    pure $ "goto L" <> show (simplify r (Add (Set (Immediate 1.0)) expression)) <> ";"

compileNormal :: Int -> ExpressionInstruction -> Tracker String
compileNormal ipRegister {expression, outputRegister} = do
    r <- get
    let e = simplify r expression
    pure $ "r[" <> show outputRegister <> "] = " <> show e <> ";"
