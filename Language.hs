module Language where
-- Description: Final Project for CS 381 Winter 2020
-- Authors:
--  > Faaiq Waqar (waqarf)
--  > Jonathan Alexander (alexajon)
--  > Julian Fortune (fortunej)

import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Data.Typeable
import           System.IO
import           Prelude                 hiding ( EQ
                                                , LT
                                                , List
                                                , and
                                                , or
                                                , subtract
                                                )


--------------------------------------------------------------
-- Core Language
--------------------------------------------------------------

----- Extension: Lists inside of lists -----
-- type List a = [a]

-- data List = SubList List
--           | IntList [Int]
--           | FloatList [Float]
--           | BoolList [Bool]
--     deriving Show
--------------------------------------------

-- A literal value that can be stored in a variable or passed as a parameter.
data VarVal = Int Int
            | Float Float
            | Boolean Bool
            | Character Char
            | IntList [Int]
            | FloatList [Float]
            | BoolList [Bool]
            | String [Char]
    deriving Show

-- ??
data Type = TInt
          | TFlt
          | TBool
          | TChar
          | TIntList
          | TFltList
          | TBoolList
          | TString
    deriving Show

-- ??
data CompVal = Loaded | Syntaxerror | Datatypeerror
    deriving Show

-- Maps variable names to the stored value.
type VarAssociation = Map.Map String VarVal

-- The data associated with a function.
data FuncData = FuncDataCon [String] [Type] Prog
  deriving Show

-- The types associated with a function.
data FuncTypeData = FuncTypeDataCon [Type] Type

-- Maps function names to the desired function.
type FuncAssociation = Map.Map String FuncData

-- Maps function names to the type of the function.
type FuncTypeAssociation = Map.Map String FuncData

-- Maps variable names to the type of the variable.
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
          | Concat Expr Expr -- Concatenate two lists
          | Cast Expr
          | Function String [Expr] -- Calls a function.
  deriving Show

-- Cmd's modify state and may modify the program flow.
data Cmd = Def String FuncData -- Define a function with a name specified by String, with parameters FuncData ex: `a = 5`.
         | Set String Expr -- Assign the VarVal specified by Expr to the variable named by the String ex: `a = 5`.
         | Insert String Expr Expr -- Assign a value to a specific element in a list ex: `myList[3] = 4`. (params: list name, index, value)
         | Delete String Expr -- Deletes the element in list variable (w/ name `String`) at index (`Expr`). (params: list name, index)
         | If Expr Prog
         | While Expr Prog
         | ForEach String Expr Prog -- Iterates and performs block of code for each item in a list. Ex: For <var> in <list> { <prog> }
         | Return Expr
  deriving Show

data MaybeError x = Result x
                  | Error String
  deriving Show

data ErrorLine = Line Int
                | NoError
  deriving Show

-- A program is composed a list of commands.
type Prog = [Cmd]

type FunctionName = String

type CompileState = (CompVal, String, ErrorLine, FunctionName)

type CompileStatus = [CompileState]

-- A program state includes the variables, functions, and the program itself.
data State = ProgState VarAssociation FuncAssociation Prog
  deriving Show

data TypeState = ProgTypeState VarTypeAssociation FuncAssociation Prog
  deriving Show

--------------------------------------------------------------
-- Syntactic sugar
--------------------------------------------------------------

or :: Expr -> Expr -> Expr
or left right = Not (Operation And (Not left) (Not right)) -- Demorgan's law babyyy :)

lessOrEqual :: Expr -> Expr -> Expr
lessOrEqual left right =
  or (Operation Less (left) (right)) (Operation Equal (left) (right))

greater :: Expr -> Expr -> Expr
greater left right = Not (lessOrEqual left right)

greaterOrEqual :: Expr -> Expr -> Expr
greaterOrEqual left right = Not (Operation Less left right)

-- i.e.: list[index] = value
assign :: String -> Expr -> Expr -> Cmd
assign list index value =
  If (Literal (Boolean True)) [Delete list index, Insert list index value]


--------------------------------------------------------------
-- Built-in Library
--------------------------------------------------------------

prelude :: Prog
prelude =
  [ Def -- Append: Takes list and element and returns list with element appended. (This will break when typeSystem is implemented)
    "append"
    (FuncDataCon
      ["list", "element"]
      [TIntList, TInt]
      [ Set "index" (Length "list") -- Get length of list
      , Insert "list" (Variable "index") (Variable "element")
      , Return (Variable "list")
      ]
    )
  , Def -- Range: Takes and int and generates Int list [0...input].
    "range"
    (FuncDataCon
      ["count"]
      [TInt]
      [ Set "index" (Literal (Int 0)) -- Get length of list
      , Set "list"  (Literal (IntList []))
      , While
        (Operation Less (Variable "index") (Variable "count"))
        [ Set "list"  (Function "append" [Variable "list", Variable "index"]) -- Append
        , Set "index" (Operation Add (Variable "index") (Literal (Int 1))) -- Increment index
        ]
      , Return (Variable "list")
      ]
    )
  ]

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
getFuncProg (FuncDataCon _ _ prog) = prog

-- Utility function to get an arg name list out of the function map data
getFuncArgs :: FuncData -> [String]
getFuncArgs (FuncDataCon args types _) = args

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
  Error
    "Type Error: `less than` is not defined for mismatched or non-numeric types."

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

element :: [a] -> Int -> Maybe a
element []            _ = Nothing -- Out of range
element (elem : list) 0 = Just elem -- Desired
element (elem : list) i = element list (i - 1)

getElement :: VarVal -> Int -> MaybeError VarVal
getElement (FloatList list) index = case element list index of
  Just value -> Result (Float value)
  Nothing    -> Error "Index Error: Index out of range."
getElement (IntList list) index = case element list index of
  Just value -> Result (Int value)
  Nothing    -> Error "Index Error: Index out of range."
getElement (BoolList list) index = case element list index of
  Just value -> Result (Boolean value)
  Nothing    -> Error "Index Error: Index out of range."
getElement (String list) index = case element list index of
  Just value -> Result (Character value)
  Nothing    -> Error "Index Error: Index out of range."
getElement _ _ = Error "Type Error: List access only permitted for lists."

getLength :: VarVal -> MaybeError VarVal
getLength (FloatList list) = Result (Int (length list))
getLength (IntList   list) = Result (Int (length list))
getLength (BoolList  list) = Result (Int (length list))
getLength (String    list) = Result (Int (length list))
getLength _ = Error "Type Error: Length only permitted for lists."

concatenateLists :: VarVal -> VarVal -> MaybeError VarVal
concatenateLists (IntList first) (IntList second) =
  Result (IntList (first ++ second))
concatenateLists (FloatList first) (FloatList second) =
  Result (FloatList (first ++ second))
concatenateLists (BoolList first) (BoolList second) =
  Result (BoolList (first ++ second))
concatenateLists (String first) (String second) =
  Result (String (first ++ second))
concatenateLists _ _ =
  Error "Type Error: Concatenation is only allowed between the same list types."

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
  _        -> Error ("Variable '" ++ name ++ "' reference before assignment.")
exprEval _ (Literal val) = Result val
exprEval (ProgState vars funcs p) (Element name index) =
  case
      ( exprEval (ProgState vars funcs p) (Variable name) -- Look up list
      , exprEval (ProgState vars funcs p) index
      )
    of
      (Result list, Result (Int i)) -> case getElement list i of
        Result value -> Result value
        Error  s     -> Error s
      (Result list, Result (_)) -> Error "Index must be an Int."
      (_          , Error s   ) -> Error s
exprEval (ProgState vars funcs p) (Length name) =
  case (exprEval (ProgState vars funcs p) (Variable name)) of -- Look up list
    (Result list) -> case getLength list of
      Result value -> Result value
      Error  s     -> Error s
    (Error s) -> Error s
exprEval (ProgState vars funcs p) (Concat left right) =
  case
      ( exprEval (ProgState vars funcs p) left
      , exprEval (ProgState vars funcs p) right
      )
    of -- Look up list
      (Result list1, Result list2) -> case concatenateLists list1 list2 of
        Result newList -> Result newList
        Error  s       -> Error s
      (Error s, _      ) -> Error s
      (_      , Error s) -> Error s
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

-- Params: List, Insert, Value, New List
insert :: [a] -> Int -> a -> Maybe [a]
insert list          0 v = Just (v : list)
insert (head : list) i v = case insert list (i - 1) v of
  Just newList -> Just (head : newList)
  Nothing      -> Nothing
insert [] _ _ = Nothing -- Out of range

-- Parameters:  List      Insert      Value     New List
insertInList :: VarVal -> Int -> VarVal -> MaybeError VarVal
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
insertInList (String list) index (Character value) =
  case insert list index value of
    Just newList -> Result (String newList)
    Nothing      -> Error "Index Error: Index out of range"
-- Type error
insertInList _ _ _ = Error "Type Error: Mismatch when inserting in list."

-- Params: List, Insert, Value, New List
delete :: [a] -> Int -> Maybe [a]
delete (head : list) 0 = Just list -- Remove element
delete (head : list) i = case delete list (i - 1) of
  Just newList -> Just (head : newList)
  Nothing      -> Nothing
delete [] _ = Nothing -- Out of range

-- Parameters:  List      Insert  New List
deleteFromList :: VarVal -> Int -> MaybeError VarVal
-- Delete from a Float list
deleteFromList (FloatList list) index = case delete list index of
  Just newList -> Result (FloatList newList)
  Nothing      -> Error "Index Error: Index out of range"
-- Delete from an Int list
deleteFromList (IntList list) index = case delete list index of
  Just newList -> Result (IntList newList)
  Nothing      -> Error "Index Error: Index out of range"
-- Delete from a Bool list
deleteFromList (BoolList list) index = case delete list index of
  Just newList -> Result (BoolList newList)
  Nothing      -> Error "Index Error: Index out of range"
-- Delete character from string
deleteFromList (String list) index = case delete list index of
  Just newList -> Result (String newList)
  Nothing      -> Error "Index Error: Index out of range"

-- Maps a variable name, list of varVals, and block of code and copies the block for
-- each item, setting the name to hold each element in each block.
forEachProgram :: String -> [VarVal] -> Prog -> Prog
forEachProgram _ [] _ = []
forEachProgram name (element : list) block =
  blockInForEach name element block ++ forEachProgram name list block

 -- Utility helper for forEachProgram that produces a program: assign element to var (String), block of code
blockInForEach :: String -> VarVal -> Prog -> Prog
blockInForEach name element innerBlock =
  Set name (Literal element) : innerBlock

-- Evaluate currently executing command. Loops and Conditionals are handled by injecting commands onto the current state's program.
cmd :: State -> Cmd -> MaybeError (State, Maybe VarVal)
cmd (ProgState vars funcs p) (Def name funcdata) =
  Result (ProgState vars (Map.insert name funcdata funcs) p, Nothing)
cmd (ProgState vars funcs p) (Set name val) =
  case exprEval (ProgState vars funcs p) val of
    Result v -> Result (ProgState (Map.insert name v vars) funcs p, Nothing)
    Error  s -> Error s
cmd (ProgState vars funcs p) (Insert name index val) =
  case
      ( exprEval (ProgState vars funcs p) (Variable name)
      , exprEval (ProgState vars funcs p) index
      , exprEval (ProgState vars funcs p) val
      )
    of
      (Result l, Result (Int i), Result v) -> case insertInList l i v of -- Try to perform the insertion
        Result newList ->  -- The insertion was successful, and created a new list with the element inserted.
          Result (ProgState (Map.insert name newList vars) funcs p, Nothing)
        Error s -> Error s -- The insertion failed.
      (Result l, Result (_), Result v) -> Error "Insert must be an Int."
      (Error  s, _         , _       ) -> Error s
      (_       , Error s   , _       ) -> Error s
      (_       , _         , Error s ) -> Error s
cmd (ProgState vars funcs p) (Delete name index) =
  case
      ( exprEval (ProgState vars funcs p) (Variable name)
      , exprEval (ProgState vars funcs p) index
      )
    of
      (Result l, Result (Int i)) -> case deleteFromList l i of -- Try to perform the insertion
        Result newList ->  -- The deletion was successful, and created a new list with the element deleted.
          Result (ProgState (Map.insert name newList vars) funcs p, Nothing)
        Error s -> Error s -- The deletion failed
      (Result l, Result (_)) -> Error "Insert must be an Int."
      (Error  s, _         ) -> Error s
      (_       , Error s   ) -> Error s
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
cmd (ProgState vars funcs p) (ForEach iterName iterOver block) =
  case exprEval (ProgState vars funcs p) iterOver of
    Result (FloatList list) -> Result
      ( ProgState vars
                  funcs
                  (forEachProgram iterName (map Float list) block ++ p)
      , Nothing
      )
    Result (IntList list) -> Result
      ( ProgState vars funcs (forEachProgram iterName (map Int list) block ++ p)
      , Nothing
      )
    Result (BoolList list) -> Result
      ( ProgState vars
                  funcs
                  (forEachProgram iterName (map Boolean list) block ++ p)
      , Nothing
      )
    Result (String list) -> Result
      ( ProgState vars
                  funcs
                  (forEachProgram iterName (map Character list) block ++ p)
      , Nothing
      )
    Error s -> Error s
    _       -> Error "For-each loop may only be applied to a list."
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

-- Runs a program *WITHOUT* type checking by initializing an empty state and processing the program.
run :: Prog -> MaybeError VarVal
run p = prog (ProgState Map.empty Map.empty (prelude ++ p)) -- Adds prelude functions

-- Compiler Reinstate - Works on Syntactic Sugar Now

compile :: Prog -> String
compile p =
  case
      (findLine (ProgState Map.empty Map.empty (prelude ++ p)) (-1) ("Main"))
    of
      [] -> pretty
        ([ ( Loaded
           , "Use the function run to start the program"
           , NoError
           , "Main"
           )
         ]
        )
      _ -> pretty
        (concatenator
          (findLine (ProgState Map.empty Map.empty (prelude ++ p)) (-1) ("Main")
          )
        )

-- Concatenator - Compiles the error codes into a compile status that we can pretty-fy
concatenator :: [(Int, String, String)] -> CompileStatus
concatenator [] = []
concatenator ((c, s, f) : xs) =
  [(Syntaxerror, s, Line c, f)] ++ concatenator xs

-- Use this to itereatively find a line of a program in which the error is located, and what function
findLine :: State -> Int -> String -> [(Int, String, String)]
findLine (ProgState _ _ []) _ _ = []
findLine (ProgState vars funcs ((Def str (FuncDataCon v l f)) : xs)) c t = -- Special case pattern - if we define a neew function, prepare to check it
  case (cmd (ProgState vars funcs xs) (Def str (FuncDataCon v l f))) of
    Error s -> [(c, s, t)] ++ findLine (ProgState vars funcs xs) (c + 1) t
    Result ((ProgState a b z), Nothing) ->
      (findLine (newFuncState (ProgState Map.empty b f) v l) 1 str)
        ++ (findLine (ProgState a b z) (c + 1) t)
    Result (_, Just _) -> []
findLine (ProgState vars funcs (x : xs)) c t =
  case (cmd (ProgState vars funcs xs) x) of
    Error s -> [(c, s, t)] ++ findLine (ProgState vars funcs xs) (c + 1) t
    Result (newstate, Nothing) -> findLine newstate (c + 1) t
    Result (_, Just _) -> []

-- Use this to build a working state for functions to be checked for errors based on type
newFuncState :: State -> [String] -> [Type] -> State
newFuncState curState                     []       []       = curState
newFuncState (ProgState types funcs prog) (s : ss) (t : ts) = case t of
  TInt ->
    newFuncState (ProgState (Map.insert s (Int 5) types) funcs prog) ss ts
  TFlt ->
    newFuncState (ProgState (Map.insert s (Float 5.5) types) funcs prog) ss ts
  TBool -> newFuncState
    (ProgState (Map.insert s (Boolean True) types) funcs prog)
    ss
    ts
  TIntList -> newFuncState
    (ProgState (Map.insert s (IntList [1, 2, 3, 4, 5]) types) funcs prog)
    ss
    ts
  TFltList -> newFuncState
    (ProgState (Map.insert s (FloatList [1.5, 2.5, 3.5, 4.5, 5.5]) types)
               funcs
               prog
    )
    ss
    ts
  TBoolList -> newFuncState
    (ProgState (Map.insert s (BoolList [True, False, True]) types) funcs prog)
    ss
    ts

-- Make it look nice and like a real readable compiler!
pretty :: CompileStatus -> String
pretty [] = "\n\n"
pretty ((Loaded, s, _, f) : xs) =
  "Program Loaded\n" ++ s ++ "\n" ++ f ++ " Function\n\n"
pretty ((_, s, (Line i), f) : xs) =
  "Error: "
    ++ s
    ++ "\nFound on line "
    ++ (show i)
    ++ " of Function: "
    ++ f
    ++ "\n\n"
    ++ pretty xs

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
-- exprType (ProgTypeState vars funcs p) (Function name args) = -- Might need to change this case to work with new function definition
--   case Map.lookup name funcs of
--     Just (FuncTypeData argtypes returntype) -> case checkFuncArgsType (ProgTypeState vars funcs p) args argtypes of
--       True -> Just returntype
--       False -> Nothing
--     _ -> Nothing

-- cmdType :: TypeState -> Cmd -> Maybe (State, Maybe Type)
-- cmdType oldstate (Def name funcdata) = case progType (buildFuncTypeState oldstate ) of -- This case is non-functional, needs to be rewritten to work with new function definition
--   Just t -> Just (ProgState vars (Map.insert name (FuncTypeData fff t)))
--   Nothing -> Nothing
-- cmdType (ProgTypeState vars funcs p) (Set name val) =
--   case exprType (ProgTypeState vars funcs p) val of
--     Just t -> case Map.lookup name vars of
--       Just u -> case t == u of
--         True -> Just (ProgTypeState vars funcs p, Nothing)
--         False -> Nothing
--       Nothing -> Just (ProgTypeState (Map.insert name t vars) funcs p, Nothing)
--     Nothing -> Nothing
-- cmdType (ProgTypeState vars funcs p) (If condition block) = -- This case probably won't work, maybe prog is the wrong thing to call here.  possibly new function needed?  Issue here is that prog returns Error or a Type, and an if statement block doesn't necessarily return anything.
--   case exprEval (ProgTypeState vars funcs p) condition of
--     Just TBool -> case progType (ProgTypeState vars funcs p) of
--       Just t -> Just ((ProgTypeState vars funcs p), Just t)
--       Nothing -> Nothing
--     Nothing                      -> Nothing
-- cmdType (ProgTypeState vars funcs p) (While condition block) = -- Same as the above comment
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




-- Compile the language to check for semantic errors that may occur such as datatype and syntax errors
-- Compile - Primary function evaluating commands from a program
{-
isLoaded :: CompVal -> Bool
isLoaded Loaded = True
isLoaded _      = False

isSyntaxError :: CompVal -> Bool
isSyntaxError Syntaxerror = True
isSyntaxError _           = False

isDataError :: CompVal -> Bool
isDataError Datatypeerror = True
isDataError _             = False

checkVarVal :: Expr -> Bool
checkVarVal (Literal _) = True
checkVarVal _           = False

-- compile :: Prog -> CompVal

-- Used to evaluate literal values to match with other booleans

compileVarVal :: VarVal -> CompVal
compileVarVal _ = Loaded

compileFuncData :: FuncData -> CompVal

compileFuncType :: FuncTypeData -> CompVal

compileOperation :: Operation -> CompVal
compileOperation _ -> CompVal

-- Compile Expr - Used to parse through expressiosn

compileExpr :: Expr -> CompVal
compileExpr (Operation opr exprone exprtwo) =
	case of opr
		Add -> case of (typeOf exprone == typeOf exprtwo)
			True -> case of (typeOf exprone)
				Int x -> Loaded
				Float x -> Loaded
				_ -> Datatypeerror
			_ -> Datatypeerror
		Sub -> case of (typeOf exprone == typeOf exprtwo)
			True -> case of (typeOf exprone)
				Int x -> Loaded
				Float x -> Loaded
				_ -> Datatypeerror
			_ -> Datatypeerror
		Mul -> case of (typeOf exprone == typeOf exprtwo)
			True -> case of (typeOf exprone)
				Int x -> Loaded
				Float x -> Loaded
				_ -> Datatypeerror
			_ -> Datatypeerror
		Div -> case of (typeOf exprone == typeOf exprtwo)
			True -> case of (typeOf exprone)
				Int x -> Loaded
				Float x -> Loaded
				_ -> Datatypeerror
			_ -> Datatypeerror
		Equal -> case of (typeOf exprone == typeOf exprtwo)
			True -> case of (typeOf exprone)
				Int x -> Loaded
				Float x -> Loaded
				Boolean x -> Loaded
				_ -> Datatypeerror
			_ -> Datatypeerror
		Less -> case of (typeOf exprone == typeOf exprtwo)
			True -> case of (typeOf exprone)
				Int x -> Loaded
				Float x -> Loaded
				_ -> Datatypeerror
			_ -> Datatypeerror
		And -> case of (typeOf exprone == typeOf exprtwo)
			True -> case of (typeOf exprone)
				Boolean x -> Loaded
				_ -> Datatypeerror
			_ -> Datatypeerror

compileExpr (Not expr) =
	case of (typeOf expr)
		Boolean x -> Loaded
		_ 	  -> Datatypeerror

compileExpr (Literal VarVal) = Loaded
compileExpr (Element str expr) = compileExpr expr
compileExpr (Length str) = Loaded
compileExpr (Function str exprs) = compileExprs exprs

compileExprs :: [Expr] -> CompVal
compileExprs [] = Loaded
compileExprs (x:xs) =
	case of (isLoaded(compileExpr x) && isLoaded(compileExprs xs))
		True -> Loaded
		_    -> Syntaxerror

-- Compile CmD - Used to parse through command expressions

compileCmd :: Cmd -> CompVal
compileCmd (Def str fdta) = compileFuncData fdta
compileCmd (Set str expr) =
	case of (checkVarVal(expr))
		True -> Loaded
		_    -> Datatypeerror
compileCmd (If expr prog) =
	case of (isLoaded(compileExpr expr) && isLoaded(compile prog))
		True -> Loaded
		_    -> Syntaxerror
compileCmd (While expr prog) =
	case of (isLoaded(compileExpr expr) && isLoaded(compile prog))
		True -> Loaded
		_    -> Syntaxerror
compileCmd (Macro cmd) = compileCmd cmd
compileCmd (Return expr) = compileExpr expr
-}
