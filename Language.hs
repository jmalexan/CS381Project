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
                                                , and
                                                , or
                                                , subtract
                                                )

--------------------------------------------------------------
-- Core Language
--------------------------------------------------------------

data VarVal = Int Integer | Float Float | Boolean Bool | IntList [Integer] | FloatList [Float] | BoolList [Bool]
    deriving Show

data Type = TInt | TFlt | TBool | TIntList | TFltList | TBoolList
    deriving Show

data CompVal = Loaded | Syntaxerror | Datatypeerror
    deriving Show

type VarAssociation = Map.Map String VarVal
data FuncData = FuncDataCon [String] Prog
  deriving Show
data FuncTypeData = FuncTypeDataCon [Type] Type
type FuncAssociation = Map.Map String FuncData
type FuncTypeAssociation = Map.Map String FuncData

type VarTypeAssociation = Map.Map String Type

-- Numeric (only works for Floats and Ints) operations
data Operation = Add | Sub | Mul | Div | Equal | Less | And
    deriving Show

-- Expressions evaluate to VarVal's.
data Expr = Operation Operation Expr Expr -- Applies the operation to the 2 expressions to produce a VarVal.
          | Not Expr -- Literal value. Negates Bool values.
          | Variable String -- References a variable by the name (String). Evaluates to the value of the variable.
          | Literal VarVal -- Literal value.
          | Element String Expr  -- Fetch the value of a specific element in a list ex: `a = b + myList[3]`
          | Length String -- Get the length of a list
          | Function String [Expr] -- Calls a function.
  deriving Show

-- Cmd's modify state and may modify the program flow.
data Cmd = Def String FuncData -- Define a function with a name specified by String, with parameters FuncData ex: `a = 5`.
         | Set String Expr -- Assign the VarVal specified by Expr to the variable named by the String ex: `a = 5`.
         | Index String Expr Expr -- Assign a value to a specific element in a list ex: `myList[3] = 4`.
         | If Expr Prog
         | While Expr Prog
         | Return Expr
  deriving Show

data MaybeError x = Result x
                  | Error String
  deriving Show

-- A program is composed a list of commands.
type Prog = [Cmd]

-- A program state includes the variables, functions, and the program itself.
data State = ProgState VarAssociation FuncAssociation Prog
  deriving Show

data TypeState = ProgTypeState VarTypeAssociation FuncAssociation Prog
  deriving Show

--------------------------------------------------------------
-- Syntactic sugar
--------------------------------------------------------------

or :: Expr -> Expr -> Expr
or x y = Not (Operation And (Not x) (Not y)) -- Demorgan's law babyyy :)

-- TODO: For each

--------------------------------------------------------------
-- Built-in Library
--------------------------------------------------------------

-- TODO: Append ?

-- TODO: Prepend ?

--------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------

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

-- Perform the add operation.
add :: VarVal -> VarVal -> MaybeError VarVal
add (Int   x) (Int   y) = Result (Int (x + y))
add (Float x) (Float y) = Result (Float (x + y))
add _ _ =
  Error "Type Error: `add` is not defined for mismatched or non-numeric types."

-- Perform the sub operation.
subtract :: VarVal -> VarVal -> MaybeError VarVal
subtract (Int   x) (Int   y) = Result (Int (x - y))
subtract (Float x) (Float y) = Result (Float (x - y))
subtract _ _ =
  Error "Type Error: `sub` is not defined for mismatched or non-numeric types."

-- Perform the mul operation.
multiply :: VarVal -> VarVal -> MaybeError VarVal
multiply (Int   x) (Int   y) = Result (Int (x * y))
multiply (Float x) (Float y) = Result (Float (x * y))
multiply _ _ =
  Error "Type Error: `mul` is not defined for mismatched or non-numeric types."

-- Perform the div operation.
divide :: VarVal -> VarVal -> MaybeError VarVal
divide (Int   x) (Int   y) = Result (Int (div x y))
divide (Float x) (Float y) = Result (Float (x / y))
divide _ _ =
  Error "Type Error: `div` is not defined for mismatched or non-numeric types."

-- Perform the equal operation.
equal :: VarVal -> VarVal -> MaybeError VarVal
equal (Int     x) (Int     y) = Result (Boolean (x == y))
equal (Float   x) (Float   y) = Result (Boolean (x == y))
equal (Boolean x) (Boolean y) = Result (Boolean (x == y))
equal _ _ = Error "Type Error: `mul` is not defined for mismatched types."

-- Perform the less than operation.
less :: VarVal -> VarVal -> MaybeError VarVal
less (Int   x) (Int   y) = Result (Boolean (x < y))
less (Float x) (Float y) = Result (Boolean (x < y))
less _ _ =
  Error "Type Error: `mul` is not defined for mismatched or non-numeric types."

-- Perform the and operation.
and :: VarVal -> VarVal -> MaybeError VarVal
and (Boolean x) (Boolean y) = Result (Boolean (x && y))
and _ _ = Error "Type Error: `and` is only defined for boolean types."

-- Applies an operataion to a pair of values
operationEval :: Operation -> VarVal -> VarVal -> MaybeError VarVal
operationEval Add   x y = add x y
operationEval Sub   x y = subtract x y
operationEval Mul   x y = multiply x y
operationEval Div   x y = divide x y
operationEval Equal x y = equal x y
operationEval Less  x y = less x y
operationEval And   x y = and x y

element :: [a] -> Integer -> Maybe a
element []            _ = Nothing -- Out of range
element (elem : list) 0 = Just elem -- Desired
element (elem : list) i = element list (i - 1)

getElement :: VarVal -> Integer -> MaybeError VarVal
getElement (FloatList list) index = case element list index of
  Just value -> Result (Float value)
  Nothing    -> Error "Index Error: Index out of range"
getElement (IntList list) index = case element list index of
  Just value -> Result (Int value)
  Nothing    -> Error "Index Error: Index out of range"
getElement (BoolList list) index = case element list index of
  Just value -> Result (Boolean value)
  Nothing    -> Error "Index Error: Index out of range"

-- Evaluates an expression to produce a VarVal or, if something is wrong, an Error.
exprEval :: State -> Expr -> MaybeError VarVal
exprEval oldstate (Operation oper expr1 expr2) =
  case (exprEval oldstate expr1, exprEval oldstate expr2) of
    (Result x, Result y) -> operationEval oper x y
    (Error  s, _       ) -> Error s
    (_       , Error s ) -> Error s
exprEval oldstate (Not expr) = case exprEval oldstate expr of
  Result (Boolean True ) -> Result (Boolean False)
  Result (Boolean False) -> Result (Boolean True)
  _                      -> Error "`not` is only defined for Boolean values."
exprEval (ProgState vars _ _) (Variable name) = case Map.lookup name vars of
  Just val -> Result val
  _        -> Error ("Variable '" ++ name ++ "'reference before assignment.")
exprEval _ (Literal val) = Result val
exprEval (ProgState vars funcs p) (Element name index) =
  case (Map.lookup name vars, exprEval (ProgState vars funcs p) index) of
    (Just list, Result (Int i)) -> case getElement list i of
      Result value -> Result value
      Error  s     -> Error s
    (Just list, Result (_)) -> Error "Index must be an Int."
    (Nothing, _) ->
      Error ("Variable '" ++ name ++ "'reference before assignment.")
    (_, Error s) -> Error s
exprEval (ProgState vars funcs p) (Function name args) =
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
    _ -> Error ("Function '" ++ name ++ "' undefined")

-- Params: List, Index, Value, New List
insert :: [a] -> Integer -> a -> Maybe [a]
insert list          0 v = Just (v : list)
insert (head : list) i v = case insert list (i - 1) v of
  Just newList -> Just (head : newList)
  Nothing      -> Nothing
insert [] _ _ = Nothing -- Out of range

-- Parameters:  List      Index      Value     New List
insertInList :: VarVal -> Integer -> VarVal -> MaybeError VarVal
-- Insert a Float into a Float list
insertInList (FloatList list) index (Float value) =
  case insert list index value of
    Just newList -> Result (FloatList newList)
    Nothing      -> Error "Index Error: Index out of range"
-- Insert an Int into an Int list
insertInList (IntList list) index (Int value) = case insert list index value of
  Just newList -> Result (IntList newList)
  Nothing      -> Error "Index Error: Index out of range"
-- Insert a Bool into a Bool list
insertInList (BoolList list) index (Boolean value) =
  case insert list index value of
    Just newList -> Result (BoolList newList)
    Nothing      -> Error "Index Error: Index out of range"
-- Type error
insertInList _ _ _ = Error "Type Error: Mismatch when inserting in list."

-- Evaluate currently executing command. Loops and Conditionals are handled by injecting commands onto the current state's program.
cmd :: State -> Cmd -> MaybeError (State, Maybe VarVal)
cmd (ProgState vars funcs p) (Def name funcdata) =
  Result (ProgState vars (Map.insert name funcdata funcs) p, Nothing)
cmd (ProgState vars funcs p) (Set name val) =
  case exprEval (ProgState vars funcs p) val of
    Result v -> Result (ProgState (Map.insert name v vars) funcs p, Nothing)
    Error  s -> Error s
cmd (ProgState vars funcs p) (Index name index val) =
  case
      ( exprEval (ProgState vars funcs p) (Variable name)
      , exprEval (ProgState vars funcs p) index
      , exprEval (ProgState vars funcs p) val
      )
    of
      (Result l, Result (Int i), Result v) -> case insertInList l i v of -- Try to perform the insertion
        Result newList ->  -- The insertion was successful, and created a new list with the element inserted
          Result (ProgState (Map.insert name newList vars) funcs p, Nothing)
        Error s -> Error s -- The insertion failed
      (Result l, Result (_), Result v) -> Error "Index must be an Int."
      (Error  s, _         , _       ) -> Error s
      (_       , Error s   , _       ) -> Error s
      (_       , _         , Error s ) -> Error s
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



-- -- Builds a new state object for use in a function call.  Takes arguments in this order: current program state, list of expr to fill args, list of arg names, empty var map (to be built), function definitions (to be passed), program block to execute
-- buildFuncTypeState
--   :: TypeState
--   -> [Expr]
--   -> [String]
--   -> VarTypeAssociation 
--   -> FuncTypeAssociation
--   -> Prog
--   -> Maybe TypeState
-- buildFuncTypeState _ [] [] vars funcs p = Just (ProgTypeState vars funcs p)
-- buildFuncTypeState _ [] _  vars funcs p = Nothing
-- buildFuncTypeState _ _  [] vars funcs p = Nothing
-- buildFuncTypeState oldstate (x : xs) (s : ss) vars funcs p =
--   case exprType oldstate x of
--     Result v -> buildFuncTypeState oldstate xs ss (Map.insert s v vars) funcs p
--     Error  s -> Error s

-- -- Check the add operation types.
-- addType :: Type -> Type -> Maybe Type
-- addType TBool TBool = Nothing
-- addType x x = Just x
-- addType _ _ = Nothing

-- -- Check the sub operation types.
-- subtractType :: Type -> Type -> Maybe Type
-- subtractType TBool TBool = Nothing
-- subtractType x x = Just x
-- subtractType _ _ = Nothing

-- -- Check the mul operation types.
-- multiplyType :: Type -> Type -> Maybe Type
-- multiplyType TBool TBool = Nothing
-- multiplyType x x = Just x
-- multiplyType _ _ = Nothing

-- -- Check the div operation types.
-- divideType :: Type -> Type -> Maybe Type
-- divideType TBool TBool = Nothing
-- divideType x x = Just x
-- divideType _ _ = Nothing

-- -- Check the equal operation types.
-- equalType :: Type -> Type -> Maybe Type
-- equalType x x = Just TBool
-- equalType _ _ = Nothing

-- -- Check the less than operation types.
-- lessType :: Type -> Type -> Maybe Type
-- lessType TBool TBool = Nothing
-- lessType x x = Just x
-- lessType _ _ = Nothing

-- -- Check the and operation types.
-- andType :: VarVal -> VarVal -> MaybeError VarVal
-- andType TBool TBool = Just TBool
-- andType _ _ = Nothing

-- -- Checks the types of an operation on two values.
-- operationType :: Operation -> Type -> Type -> Maybe Type
-- operationType Add   x y = add x y
-- operationType Sub   x y = subtract x y
-- operationType Mul   x y = multiply x y
-- operationType Div   x y = divide x y
-- operationType Equal x y = equal x y
-- operationType Less  x y = less x y
-- operationType And   x y = and x y


-- checkFuncArgsType :: TypeState -> [Expr] -> [Type] -> Boolean
-- checkFuncArgsType _ [] [] = True
-- checkFuncArgsType _ [] _ = False
-- checkFuncArgsType _ _ [] = False
-- checkFuncArgsType s (x:xs) (y:ys) = case exprType s x of
--   Just z -> case z == y of
--     True  -> checkFuncArgsType s xs ys
--     False -> False
--   _      -> Nothing

-- exprType :: TypeState -> Expr -> Maybe Type
-- exprType oldstate (Operation oper expr1 expr2) = case (exprType oldstate expr1, exprType oldstate expr2) of
--   (Just x, Just y) -> operationType x y
--   _                -> Nothing
-- exprType oldstate (Not expr) = case exprType oldstate expr of
--   Just TBool -> TBool
--   _          -> Nothing
-- exprType (ProgState vars _ _) (Variable name) = Map.lookup name vars
-- exprType _ (Literal (Int _)) = Just TInt
-- exprType _ (Literal (Flt _)) = Just TFlt
-- exprType _ (Literal (Boolean _)) = Just TBool
-- exprType (ProgTypeState vars funcs p) (Function name args) =
--   case Map.lookup name funcs of
--     Just (FuncTypeData argtypes returntype) -> case checkFuncArgsType (ProgTypeState vars funcs p) args argtypes of
--       True -> Just returntype
--       False -> Nothing
--     _ -> Nothing

-- cmdType :: TypeState -> Cmd -> Maybe (State, Maybe Type)
-- cmdType oldstate (Def name funcdata) = case progType (buildFuncTypeState oldstate ) of
--   Just t -> Just (ProgState vars (Map.insert name (FuncTypeData fff t)))
--   Nothing -> Nothing
-- cmdType (ProgTypeState vars funcs p) (Set name val) =
--   case exprType (ProgTypeState vars funcs p) val of
--     Just t -> case Map.lookup name vars of
--       Just u -> case t == u of
--         True -> Just (ProgTypeState (Map.insert name u vars) funcs p, Nothing)
--         False -> Nothing
--       Nothing -> Just (ProgTypeState (Map.insert name t vars) funcs p, Nothing)
--     Nothing -> Nothing
-- cmdType (ProgTypeState vars funcs p) (If condition block) =
--   case exprEval (ProgTypeState vars funcs p) condition of
--     Just TBool -> case progType (ProgTypeState vars funcs p) of
--       Just t -> Just ((ProgTypeState vars funcs p), Just t)
--       Nothing -> Nothing
--     Nothing                      -> Nothing
-- cmdType (ProgTypeState vars funcs p) (While condition block) =
--   case exprEval (ProgTypeState vars funcs p) condition of
--     Just TBool -> case progType (ProgTypeState vars funcs p) of
--       Just t -> Just (ProgTypeState vars funcs p, Just t)
--       Nothing -> Nothing
--     Nothing                      -> Nothing
-- cmdType (ProgTypeState vars funcs p) (Return expr1) =
--   case exprType (ProgTypeState vars funcs p) expr1 of
--     Just t -> Just (ProgTypeState vars funcs p, Just t)
--     Nothing -> Nothing


-- progType :: TypeState -> Maybe Type
-- progType (ProgTypeState _    _     []      ) = Just TBool --base case
-- progType (ProgTypeState vars funcs (x : xs)) = case cmdType (ProgTypeState vars funcs xs) x of
--   Just (newstate, Nothing) -> progType newstate
--   Just (_       , Just x ) -> Just x
--   Nothing                   -> Nothing



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
expParser (_ (Float x1) (Int x2)) = Datatypeerror
expParser (_ (Int x1) (Float x2)) = Datatypeerror
expParser (_ (Float x1) (Float x2)) = Loaded
expParser (_ (Boolean x1) (Float x2)) = Datatypeerror
expParser (_ (Float x1) (Boolean x2)) = Datatypeerror
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
