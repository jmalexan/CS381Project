module Language where
-- Description: Final Project for CS 381 Winter 2020
-- Authors:
--  > Faaiq Waqar (waqarf)
--  > Jonathan Alexander (alexajon)
--  > Julian Fortune (fortunej)

import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Prelude                 hiding ( EQ
                                                , LT
                                                )

data VarVal = Int Integer | Flt Float | Boolean Bool | IntList [Integer] | FloatList [Float] | BoolList [Bool]
    deriving Show

data CompVal = Loaded | Syntaxerror | Datatypeerror
    deriving Show

type VarAssociation = Map.Map String VarVal
data FuncData = FuncDataCon [String] Prog
  deriving Show
type FuncAssociation = Map.Map String FuncData

-- Numeric (only works for Floats and Ints) operations
data NumOp = Add | Sub | Mul | Div
    deriving Show

-- Operations on numeric values that produce a boolean value
data CompNumOp = LT
    deriving Show

data CompOp = EQ
    deriving Show

data Expr = ExprNumOp NumOp Expr Expr
          | ExprBinOp CompNumOp Expr Expr
          | ExprCompOp CompOp Expr Expr
          | ExprVar String
          | ExprVal VarVal
          | ExprElement String Int  -- Fetch the value of a specific element in a list ex: `a = b + myList[3]`
          | ExprFunc String [Expr]
  deriving Show

data Cmd = Def String FuncData
         | Set String Expr
         | SetIndex String Int Expr -- Assign a value to a specific element in a list ex: `myList[3] = 4`
         | If Expr Prog
         | While Expr Prog
         | Return Expr
  deriving Show

data MaybeError x = Result x
                  | Error String
  deriving Show

type Prog = [Cmd]

data State = ProgState VarAssociation FuncAssociation Prog
  deriving Show

-- *
-- * Syntactic sugar
-- *

exprSum :: Expr -> Expr -> Expr
exprSum = ExprNumOp Add

exprDiv :: Expr -> Expr -> Expr
exprDiv = ExprNumOp Div

exprEQ :: Expr -> Expr -> Expr
exprEQ = ExprCompOp EQ

-- Builds a new state object for use in a function call.  Takes arguments in this order: current program state, list of expr to fill args, list of arg names, empty var map (to be built), function definitions (to be passed), program block to execute
buildFuncState
  :: State
  -> [Expr]
  -> [String]
  -> VarAssociation
  -> FuncAssociation
  -> Prog
  -> MaybeError State
buildFuncState _ [] [] vars funcs p = Result (ProgState vars funcs p)
buildFuncState _ [] _  vars funcs p = Error "Not enough arguments"
buildFuncState _ _  [] vars funcs p = Error "Too many arguments"
buildFuncState oldstate (x : xs) (s : ss) vars funcs p =
  case exprEval oldstate x of
    Result v -> buildFuncState oldstate xs ss (Map.insert s v vars) funcs p
    Error  s -> Error s

-- Utility function to get a prog block out of the function map data
getFuncProg :: FuncData -> Prog
getFuncProg (FuncDataCon _ prog) = prog

-- Utility function to get an arg name list out of the function map data
getFuncArgs :: FuncData -> [String]
getFuncArgs (FuncDataCon args _) = args
-- Applies a numeric operataion to a pair of Floats
floatOpEval :: NumOp -> Float -> Float -> VarVal
floatOpEval Add x y = Flt (x + y)
floatOpEval Sub x y = Flt (x - y)
floatOpEval Mul x y = Flt (x * y)
floatOpEval Div x y = Flt (x / y)

-- Applies a numeric operataion to a pair of Ints
intOpEval :: NumOp -> Integer -> Integer -> VarVal
intOpEval Add x y = Int (x + y)
intOpEval Sub x y = Int (x - y)
intOpEval Mul x y = Int (x * y)
intOpEval Div x y = Int (div x y) -- Force integer division

-- Applies a comparison operataion to a pair of Floats
floatBinOpEval :: CompNumOp -> Float -> Float -> VarVal
floatBinOpEval LT x y = Boolean (x < y)

-- Applies a comparison operataion to a pair of Ints
intBinOpEval :: CompNumOp -> Integer -> Integer -> VarVal
intBinOpEval LT x y = Boolean (x < y)

boolCompOpEval :: CompOp -> Bool -> Bool -> VarVal
boolCompOpEval EQ x y = Boolean (x == y)

floatCompOpEval :: CompOp -> Float -> Float -> VarVal
floatCompOpEval EQ x y = Boolean (x == y)

intCompOpEval :: CompOp -> Integer -> Integer -> VarVal
intCompOpEval EQ x y = Boolean (x == y)

-- ??
exprEval :: State -> Expr -> MaybeError VarVal
exprEval oldstate (ExprNumOp oper expr1 expr2) =
  case (exprEval oldstate expr1, exprEval oldstate expr2) of
    (Result (Int x), Result (Int y)) -> Result (intOpEval oper x y)
    (Result (Flt x), Result (Flt y)) -> Result (floatOpEval oper x y)
    (Error s       , _)              -> Error s
    (_             , Error s)        -> Error s
    _                                -> Error "Type Error"
exprEval oldstate (ExprBinOp oper expr1 expr2) =
  case (exprEval oldstate expr1, exprEval oldstate expr2) of
    (Result (Int x), Result (Int y)) -> Result (intBinOpEval oper x y)
    (Result (Flt x), Result (Flt y)) -> Result (floatBinOpEval oper x y)
    (Error s       , _)              -> Error s
    (_             , Error s)        -> Error s
    _                                -> Error "Type Error"
exprEval oldstate (ExprCompOp oper expr1 expr2) =
  case (exprEval oldstate expr1, exprEval oldstate expr2) of
    (Result (Int x), Result (Int y))         -> Result (intCompOpEval oper x y)
    (Result (Flt x), Result (Flt y))         -> Result (floatCompOpEval oper x y)
    (Result (Boolean x), Result (Boolean y)) -> Result (boolCompOpEval oper x y)
    (Error s       , _)                      -> Error s
    (_             , Error s)                -> Error s
    _                                        -> Error "Type Error"
exprEval (ProgState vars _ _) (ExprVar name) = case Map.lookup name vars of
  Just val -> Result val
  _        -> Error "Variable not found"
exprEval _ (ExprVal val) = Result val
exprEval (ProgState vars funcs p) (ExprFunc name args) =
  case Map.lookup name funcs of
    Just func ->
      case
          buildFuncState (ProgState vars funcs p)
                         args
                         (getFuncArgs func)
                         Map.empty
                         funcs
                         (getFuncProg func)
        of
          Result newstate -> prog newstate
          Error  s        -> Error s
    _ -> Error "Function not found"

-- Evaluate currently executing command.  Loops and Conditionals are handled by injecting commands onto the current state's program.
cmd :: State -> Cmd -> MaybeError (State, Maybe VarVal)
cmd (ProgState vars funcs p) (Def name funcdata) =
  Result (ProgState vars (Map.insert name funcdata funcs) p, Nothing)
cmd (ProgState vars funcs p) (Set name val) =
  case exprEval (ProgState vars funcs p) val of
    Result v -> Result (ProgState (Map.insert name v vars) funcs p, Nothing)
    Error  s -> Error s
cmd (ProgState vars funcs p) (If condition block) =
  case exprEval (ProgState vars funcs p) condition of
    Result (Boolean True) ->
      Result (ProgState vars funcs (block ++ p), Nothing)
    Result (Boolean False) -> Result (ProgState vars funcs p, Nothing)
    Error  s               -> Error s
    _                      -> Error "Non boolean in if condition"
cmd (ProgState vars funcs p) (While condition block) =
  case exprEval (ProgState vars funcs p) condition of
    Result (Boolean True) -> Result
      (ProgState vars funcs (block ++ [While condition block] ++ p), Nothing)
    Result (Boolean False) -> Result (ProgState vars funcs p, Nothing)
    Error  s               -> Error s
    _                      -> Error "Non boolean in while loop condition"
cmd (ProgState vars funcs p) (Return expr1) =
  case exprEval (ProgState vars funcs p) expr1 of
    Result x -> Result (ProgState vars funcs p, Just x)
    Error  e -> Error e

-- Recursively process the commands in a program while carrying through the state.
prog :: State -> MaybeError VarVal
prog (ProgState _    _     []      ) = Result (Boolean False) --base case
prog (ProgState vars funcs (x : xs)) = case cmd (ProgState vars funcs xs) x of
  Result (newstate, Nothing) -> prog newstate
  Result (_       , Just x ) -> Result x
  Error  s                   -> Error s

-- Runs a program by initializing an empty state and processing the program
run :: Prog -> MaybeError VarVal
run p = prog (ProgState Map.empty Map.empty p)

{-
-- Compile the language to check for semantic errors that may occur such as datatype and syntax errors
-- Compile - Primary function evaluating commands from a program

compile :: Prog -> CompVal
compile ((Def str fnc):xs) =
compile ((Set str exp):xs) =
compile ((If exp nprog):xs) =
compile ((While exp nprog):xs) =
compile ((Return exp):xs) =

-- FncParser - Submodule used to parse through Functions and Function Data

fncParser :: FuncData -> CompVal
fncParser =

-- ExpParser - Submodule used to parse through Expressions and Match Data
-- Errors here  - I have gotten the syntax wrong ;-; - Faaiq

expParser :: Expr -> CompVal
expParser (_ (Int x1) (Int x2)) = Loaded
expParser (_ (Flt x1) (Int x2)) = Datatypeerror
expParser (_ (Int x1) (Flt x2)) = Datatypeerror
expParser (_ (Flt x1) (Flt x2)) = Loaded
expParser (_ (Boolean x1) (Flt x2)) = Datatypeerror
expParser (_ (Flt x1) (Boolean x2)) = Datatypeerror
expParser (_ (Boolean x1) (Boolean x2)) = Loaded
expParser (_ (Int x1) (Boolean x2)) = Datat
expParser (_ (Boolean x1) (Int x2)) = Datatypeerror

expParser(ExpVar str) =
expParser(ExpVal val) =

-- IsLoaded - Submodule used to return boolean :: Useful for Combining Parsers

isLoaded :: CompVal -> Bool
isLoaded (Loaded) = True
isLoaded _ = False
-}
