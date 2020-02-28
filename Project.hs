module Project where
-- Description: Final Project for CS 381 Winter 2020
-- Authors:
--  > Faaiq Waqar (waqarf)
--  > Jonathan Alexander (alexajon)
--  > Julian Fortune (fortunej)

import qualified Data.Map.Strict as Map
import Data.Maybe
-- Library for associating keys with values.
--
-- I believe this is the best version for us because:
--  > It is designed for use with string keys
--  > It appears to perform the best
--  > We `don't care about ordering`
--  > Strict is `more efficient when laziness is not necessary`
--
-- Note: This module has some naming conflicts with Prelude (thus `qualified`)
-- Definition: `data HashMap k v = Empty | ... `
-- Source code: https://hackage.haskell.org/package/unordered-containers-0.2.10.0/docs/src/Data.HashMap.Base.html#HashMap

-- | Julian's experiments with HashMap

-- Holds different literal/storable types that could be in a variable


-- data Value = Int Integer | Str String | Flt Float
--     deriving Show

-- -- Maps a string (variable name) to a `Value`
-- type Association = Map.HashMap String Value

-- exampleHashMap :: Association
-- exampleHashMap =  ( Map.insert "c" (Flt 3.14159) .
--                     Map.insert "b" (Str "hello, world!") .
--                     Map.insert "a" (Int 6) ) Map.empty

-- exampleLookup :: Maybe Value
-- exampleLookup =  Map.lookup "b" exampleHashMap



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

type Prog = [Cmd]

data State = ProgState VarAssociation FuncAssociation Prog

mainProg :: Prog
mainProg = [Set "x" (ExprVal (Int 3)), Def "test" (FuncDataCon ["asdf"] [Set "b" (ExprSum (ExprVal (Int 777)) (ExprVar "asdf")), Set "b" (ExprDiv (ExprVar "b") (ExprVal (Int 2))), Return (ExprVar "b")]), Set "x" (ExprFunc "test" [ExprVar "x"]), If (ExprEQ (ExprVar "x") (ExprVal (Int 381))) [Set "x" (ExprVal (Boolean True))], Return (ExprVar "x")]

mainState :: State
mainState = ProgState Map.empty Map.empty mainProg

-- Builds a new state object for use in a function call.  Takes arguments in this order: current program state, list of expr to fill args, list of arg names, empty var map (to be built), function definitions (to be passed), program block to execute
buildFuncState :: State -> [Expr] -> [String] -> VarAssociation -> FuncAssociation -> Prog -> State
buildFuncState _ [] [] vars funcs p = ProgState vars funcs p
buildFuncState oldstate (x:xs) (s:ss) vars funcs p = buildFuncState oldstate xs ss (Map.insert s (exprEval oldstate x) vars) funcs p

-- Utility function to get a prog block out of the function map data
getFuncProg :: FuncData -> Prog
getFuncProg (FuncDataCon _ prog) = prog

-- Utility function to get an arg name list out of the function map data
getFuncArgs :: FuncData -> [String]
getFuncArgs (FuncDataCon args _) = args

-- Recursive function that takes any Expr and converts it to some value.  needs state to properly evaluate variables and functions.
exprEval :: State -> Expr -> VarVal
exprEval oldstate (ExprSum expr1 expr2) = case exprEval oldstate expr1 of Int x -> case exprEval oldstate expr2 of Int y -> Int (x + y)
                                                                          Flt x -> case exprEval oldstate expr2 of Flt y -> Flt (x + y)
exprEval oldstate (ExprSub expr1 expr2) = case exprEval oldstate expr1 of Int x -> case exprEval oldstate expr2 of Int y -> Int (x - y)
                                                                          Flt x -> case exprEval oldstate expr2 of Flt y -> Flt (x - y)
exprEval oldstate (ExprMul expr1 expr2) = case exprEval oldstate expr1 of Int x -> case exprEval oldstate expr2 of Int y -> Int (x * y)
                                                                          Flt x -> case exprEval oldstate expr2 of Flt y -> Flt (x * y)
exprEval oldstate (ExprDiv expr1 expr2) = case exprEval oldstate expr1 of Int x -> case exprEval oldstate expr2 of Int y -> Int (div x y)
                                                                          Flt x -> case exprEval oldstate expr2 of Flt y -> Flt (x / y)
exprEval oldstate (ExprLT expr1 expr2) = case exprEval oldstate expr1 of Int x -> case exprEval oldstate expr2 of Int y -> Boolean (x < y)
                                                                         Flt x -> case exprEval oldstate expr2 of Flt y -> Boolean (x < y)
exprEval oldstate (ExprGT expr1 expr2) = case exprEval oldstate expr1 of Int x -> case exprEval oldstate expr2 of Int y -> Boolean (x > y)
                                                                         Flt x -> case exprEval oldstate expr2 of Flt y -> Boolean (x > y)
exprEval oldstate (ExprEQ expr1 expr2) = case exprEval oldstate expr1 of Int x -> case exprEval oldstate expr2 of Int y -> Boolean (x == y)
                                                                         Flt x -> case exprEval oldstate expr2 of Flt y -> Boolean (x == y)
                                                                         Boolean x -> case exprEval oldstate expr2 of Boolean y -> Boolean (x == y)
exprEval oldstate (ExprNE expr1 expr2) = case exprEval oldstate expr1 of Int x -> case exprEval oldstate expr2 of Int y -> Boolean (x /= y)
                                                                         Flt x -> case exprEval oldstate expr2 of Flt y -> Boolean (x /= y)
                                                                         Boolean x -> case exprEval oldstate expr2 of Boolean y -> Boolean (x /= y)
exprEval (ProgState vars _ _) (ExprVar name) = case Map.lookup name vars of Just val -> val
exprEval _ (ExprVal val) = val
exprEval (ProgState vars funcs p) (ExprFunc name args) = case Map.lookup name funcs of Just func -> prog (buildFuncState (ProgState vars funcs p) args (getFuncArgs func) Map.empty funcs (getFuncProg func))

-- Evaluate currently executing command.  Loops and Conditionals are handled by injecting commands onto the current state's program.
cmd :: State -> Cmd -> State
cmd (ProgState vars funcs p) (Def name funcdata) = ProgState vars (Map.insert name funcdata funcs) p
cmd (ProgState vars funcs p) (Set name val) = ProgState (Map.insert name (exprEval (ProgState vars funcs p) val) vars) funcs p
cmd (ProgState vars funcs p) (If condition block) = case exprEval (ProgState vars funcs p) condition of Boolean True -> ProgState vars funcs (block ++ p)
                                                                                                        Boolean False -> ProgState vars funcs p
cmd (ProgState vars funcs p) (While condition block) = case exprEval (ProgState vars funcs p) condition of Boolean True -> ProgState vars funcs (block ++ [While condition block] ++ p)
                                                                                                           Boolean False -> ProgState vars funcs p

prog :: State -> VarVal
prog (ProgState _ _ []) = Boolean False --base case
prog (ProgState vars funcs ((Return expr1):xs)) = exprEval (ProgState vars funcs xs) expr1
prog (ProgState vars funcs (x:xs)) = prog (cmd (ProgState vars funcs xs) x)

compile :: Prog -> CompVal
compile ((Def str fnc):xs) = 
compile ((Set str exp):xs) = 
compile ((If exp nprog):xs) = 
compile ((While exp nprog):xs) = 
compile ((Return exp):xs) = 

fncParser :: FuncData -> CompVal

expParser :: Expr -> CompVal
expParser (_ (Int x1) (Int x2)) = Loaded
expParser (_ (Flt x1) (Int x2)) = Datatypeerror
expParser (_ (Int x1) (Flt x2)) = Datatypeerror
expParser (_ (Flt x1) (Flt x2)) = Loaded
expParser (_ (Boolean x1) (Flt x2)) = Datatypeerror
expParser (_ (Flt x1) (Boolean x2)) = Datatypeerror
expParser (_ (Boolean x1) (Boolean x2)) = Loaded
expParser (_ (Int x1) (Boolean x2)) = Datatypeerror
expParser (_ (Boolean x1) (Int x2)) = Datatypeerror