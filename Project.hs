module Project where
-- Description: Final Project for CS 381 Winter 2020
-- Authors:
--  > Faaiq Waqar (waqarf)
--  > Jonathan Alexander (alexajon)
--  > Julian Fortune (fortunej)

import qualified Data.Map.Strict               as Map
import           Data.Maybe

data VarVal = Int Integer | Flt Float | Boolean Bool
    deriving Show

data CompVal = Loaded | Syntaxerror | Datatypeerror
    deriving Show

type VarAssociation = Map.Map String VarVal
data FuncData = FuncDataCon [String] Prog
type FuncAssociation = Map.Map String FuncData

data Expr = ExprSum Expr Expr
          | ExprSub Expr Expr
          | ExprMul Expr Expr
          | ExprDiv Expr Expr
          | ExprLT Expr Expr
          | ExprGT Expr Expr
          | ExprEQ Expr Expr
          | ExprNE Expr Expr
          | ExprVar String
          | ExprVal VarVal
          | ExprFunc String [Expr]

data Cmd = Def String FuncData
         | Set String Expr
         | If Expr Prog
         | While Expr Prog
         | Return Expr

data MaybeError x = Result x
                  | Error String
  deriving Show

type Prog = [Cmd]

data State = ProgState VarAssociation FuncAssociation Prog

mainProg :: Prog
mainProg =
  [ Set "x" (ExprVal (Int 3))
  , Def
    "test"
    (FuncDataCon
      ["asdf"]
      [ Set "b" (ExprSum (ExprVal (Int 777)) (ExprVar "asdf"))
      , Set "b" (ExprDiv (ExprVar "b") (ExprVal (Int 2)))
      , Return (ExprVar "b")
      ]
    )
  , Set "x" (ExprFunc "test" [ExprVar "x"])
  , If (ExprEQ (ExprVar "x") (ExprVal (Int 381)))
       [Set "x" (ExprVal (Boolean True))]
  , Return (ExprVar "x")
  ]

mainState :: State
mainState = ProgState Map.empty Map.empty mainProg

badProg :: Prog
badProg =
  [ Set "x" (ExprVal (Int 3))
  , Def
    "test"
    (FuncDataCon
      ["asdf"]
      [ Set "b" (ExprSum (ExprVal (Int 777)) (ExprVar "asdf"))
      , Set "b" (ExprDiv (ExprVar "b") (ExprVal (Flt 2)))
      , Return (ExprVar "b")
      ]
    )
  , Set "x" (ExprFunc "test" [ExprVar "x"])
  , If (ExprEQ (ExprVar "x") (ExprVal (Int 381)))
       [Set "x" (ExprVal (Boolean True))]
  , Return (ExprVar "x")
  ]

badState :: State
badState = ProgState Map.empty Map.empty badProg

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

-- Recursive function that takes any Expr and converts it to some value.  needs state to properly evaluate variables and functions.

exprToBool
  :: MaybeError VarVal
  -> MaybeError VarVal
  -> (Integer -> Integer -> Bool)
  -> (Float -> Float -> Bool)
  -> (Bool -> Bool -> Bool)
  -> MaybeError VarVal
exprToBool (Result (Int x)) (Result (Int y)) f _ _ = Result (Boolean (f x y))
exprToBool (Result (Flt x)) (Result (Flt y)) _ g _ = Result (Boolean (g x y))
exprToBool (Result (Boolean x)) (Result (Boolean y)) _ _ h =
  Result (Boolean (h x y))
exprToBool (Error s) _         _ _ _ = Error s
exprToBool _         (Error s) _ _ _ = Error s
exprToBool _         _         _ _ _ = Error "Type error"

exprBool
  :: MaybeError VarVal
  -> MaybeError VarVal
  -> (Integer -> Integer -> Bool)
  -> (Float -> Float -> Bool)
  -> MaybeError VarVal
exprBool (Result (Int x)) (Result (Int y)) f _ = Result (Boolean (f x y))
exprBool (Result (Flt x)) (Result (Flt y)) _ g = Result (Boolean (g x y))
exprBool (Error  s      ) _                _ _ = Error s
exprBool _                (Error s)        _ _ = Error s
exprBool _                _                _ _ = Error "Type error"

exprNum
  :: MaybeError VarVal
  -> MaybeError VarVal
  -> (Integer -> Integer -> Integer)
  -> (Float -> Float -> Float)
  -> MaybeError VarVal
exprNum (Result (Int x)) (Result (Int y)) f _ = Result (Int (f x y))
exprNum (Result (Flt x)) (Result (Flt y)) _ g = Result (Flt (g x y))
exprNum (Error  s      ) _                _ _ = Error s
exprNum _                (Error s)        _ _ = Error s
exprNum _                _                _ _ = Error "Type error"

exprEval :: State -> Expr -> MaybeError VarVal
exprEval oldstate (ExprSum expr1 expr2) = exprNum (exprEval oldstate expr1)
                                                  (exprEval oldstate expr2)
                                                  (\x y -> x + y)
                                                  (\x y -> x + y)
exprEval oldstate (ExprSub expr1 expr2) = exprNum (exprEval oldstate expr1)
                                                  (exprEval oldstate expr2)
                                                  (\x y -> x - y)
                                                  (\x y -> x - y)
exprEval oldstate (ExprMul expr1 expr2) = exprNum (exprEval oldstate expr1)
                                                  (exprEval oldstate expr2)
                                                  (\x y -> x * y)
                                                  (\x y -> x * y)
exprEval oldstate (ExprDiv expr1 expr2) = exprNum (exprEval oldstate expr1)
                                                  (exprEval oldstate expr2)
                                                  (\x y -> div x y)
                                                  (\x y -> x / y)
exprEval oldstate (ExprLT expr1 expr2) = exprBool (exprEval oldstate expr1)
                                                  (exprEval oldstate expr2)
                                                  (\x y -> x < y)
                                                  (\x y -> x < y)
exprEval oldstate (ExprGT expr1 expr2) = exprBool (exprEval oldstate expr1)
                                                  (exprEval oldstate expr2)
                                                  (\x y -> x > y)
                                                  (\x y -> x > y)
exprEval oldstate (ExprEQ expr1 expr2) = exprToBool (exprEval oldstate expr1)
                                                    (exprEval oldstate expr2)
                                                    (\x y -> x == y)
                                                    (\x y -> x == y)
                                                    (\x y -> x == y)
exprEval oldstate (ExprNE expr1 expr2) = exprToBool (exprEval oldstate expr1)
                                                    (exprEval oldstate expr2)
                                                    (\x y -> x /= y)
                                                    (\x y -> x /= y)
                                                    (\x y -> x /= y)
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

prog :: State -> MaybeError VarVal
prog (ProgState _ _ []) = Result (Boolean False) --base case
prog (ProgState vars funcs (x : xs)) = case cmd (ProgState vars funcs xs) x of
  Result (newstate, Nothing) -> prog newstate
  Result (_       , Just x ) -> Result x
  Error  s                   -> Error s

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
