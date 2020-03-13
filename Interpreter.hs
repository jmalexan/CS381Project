module Interpreter where
-- Description: Executes (evaluates) programs in our language.
-- Authors:
--  > Faaiq Waqar (waqarf)
--  > Jonathan Alexander (alexajon)
--  > Julian Fortune (fortunej)

import           CoreLanguage
import           StandardLibrary
import           TypeChecker

import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Data.Typeable
import           System.IO
import           Prelude                 hiding ( and
                                                , or
                                                , subtract
                                                )
import           Data.Char                      ( ord
                                                , chr
                                                )

--------------------------------------------------------------
 -- Functions implementation
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
getFuncProg (FuncDataCon _ _ _ prog) = prog

-- Utility function to get an arg name list out of the function map data
getFuncArgs :: FuncData -> [String]
getFuncArgs (FuncDataCon args _ _ _) = args



--------------------------------------------------------------
-- Operations implementation
--------------------------------------------------------------

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



--------------------------------------------------------------
-- List implementation
--------------------------------------------------------------

-- Expressions --

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

-- Commands --

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



--------------------------------------------------------------
-- Casting
--------------------------------------------------------------

-- Converts a String to an Int
stringToInt :: String -> MaybeError Int
stringToInt string = stringToIntHelper (reverse string) 0 1

stringToIntHelper :: String -> Int -> Int -> MaybeError Int
stringToIntHelper (char : str) f m
  | (0 <= val && val <= 10) = stringToIntHelper str (f + (val * m)) (m * 10)
  | otherwise               = Error "Casting from String to Float failed."
  where val = ((ord char) - 48)
stringToIntHelper [] f _ = Result f

-- Converts a String to a Float
stringToFloat :: String -> MaybeError Float
stringToFloat string = stringToFloatHelper (reverse string) 0 False 1

stringToFloatHelper :: String -> Float -> Bool -> Int -> MaybeError Float
stringToFloatHelper ('.' : str) f True _ =
  Error "Casting from String to Float failed."
stringToFloatHelper ('.' : str) f False m =
  stringToFloatHelper str (f / (fromIntegral m)) True (1)
stringToFloatHelper (char : str) f p m
  | (0 <= val && val <= 10) = stringToFloatHelper -- No period yet
    str
    (f + (fromIntegral (val * m)))
    p
    (m * 10)
  | otherwise = Error "Casting from String to Float failed."
  where val = ((ord char) - 48)
stringToFloatHelper [] f _ _ = Result f

-- Converts String to Bool
stringToBool :: String -> MaybeError Bool
stringToBool "True"  = Result True
stringToBool "true"  = Result True
stringToBool "False" = Result False
stringToBool "false" = Result False
stringToBool _       = Error "Casting from String to Bool failed."

-- TODO
intToChar :: Int -> MaybeError Char
intToChar val | 0 <= val && val <= 127 = Result (chr val)
              | otherwise = Error "Casting from Int to Char failed."

castValueToType :: VarVal -> Type -> MaybeError VarVal
-- Int -> ?
castValueToType (Int x) (TInt ) = Result (Int x) -- Always succeeds.
castValueToType (Int x) (TFlt ) = Result (Float (fromIntegral x)) -- Always succeeds.
castValueToType (Int x) (TChar) = case intToChar x of -- Can fail.
  Result c -> Result (Character c)
  Error  s -> Error s
castValueToType (Int       x) (TString) = Result (String (show x)) -- Always succeeds.
-- Float -> ?
castValueToType (Float     x) (TInt   ) = Result (Int (floor x)) -- Always succeeds.
castValueToType (Float     x) (TFlt   ) = Result (Float x) -- Always succeeds.
castValueToType (Float     x) (TString) = Result (String (show x)) -- Always succeeds.
-- Char -> ?
castValueToType (Character x) (TInt   ) = Result (Int (ord x)) -- Always succeeds.
castValueToType (Character x) (TChar  ) = Result (Character x) -- Always succeeds.
castValueToType (Character x) (TString) = Result (String [x]) -- Always succeeds.
-- String -> ?
castValueToType (String    x) (TInt   ) = case stringToInt x of
  Result i -> Result (Int i)
  Error  s -> Error s
castValueToType (String x) (TFlt) = case stringToFloat x of
  Result i -> Result (Float i)
  Error  s -> Error s
castValueToType (String x) (TBool) = case stringToBool x of
  Result i -> Result (Boolean i)
  Error  s -> Error s
castValueToType (String x) (TString) = Result (String x) -- Always succeeds.



--------------------------------------------------------------
-- Compile (Evaluation)
--------------------------------------------------------------

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
exprEval (ProgState vars funcs p) (Cast expr newType) =
  case exprEval (ProgState vars funcs p) expr of
    (Result value) -> castValueToType value newType
    Error s        -> Error s
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

compile :: Prog -> IO ()
compile p = case typecheck (prelude ++ p) of
  Result _ -> case run (prelude ++ p) of
    Result a -> putStrLn (show a)
    -- Error  s -> putStrLn ("Runtime error:\n\n" ++ s)
    Error  s -> putStrLn (trace (prelude ++ p))
  Error s -> putStrLn (trace (prelude ++ p))


--------------------------------------------------------------
-- Beautifying errors
--------------------------------------------------------------

-- Compile! uses syntactic sugar combined with logical parsing to find out if your program will work correctly!
trace :: Prog -> String
trace p =
  case
      (  (findLine (ProgState Map.empty Map.empty (prelude ++ p)) (-1) ("Main"))
      ++ (typeLine (ProgTypeState Map.empty Map.empty (prelude ++ p))
                   (-1)
                   ("Main")
         )
      )
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
          (  (findLine (ProgState Map.empty Map.empty (prelude ++ p))
                       (-1)
                       ("Main")
             )
          ++ (typeLine (ProgTypeState Map.empty Map.empty (prelude ++ p))
                       (-1)
                       ("Main")
             )
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
findLine (ProgState vars funcs ((Def str (FuncDataCon v l rt f)) : xs)) c t = -- Special case pattern - if we define a neew function, prepare to check it
  case (cmd (ProgState vars funcs xs) (Def str (FuncDataCon v l rt f))) of
    Error s -> [(c, s, t)] ++ findLine (ProgState vars funcs xs) (c + 1) t
    Result ((ProgState a b z), Nothing) ->
      (findLine (newFuncState (ProgState Map.empty b f) v l) 1 str)
        ++ (findLine (ProgState a b z) (c + 1) t)
        ++ (funcTypeAlign str v l)
    Result (_, Just _) -> []
findLine (ProgState vars funcs (x : xs)) c t =
  case (cmd (ProgState vars funcs xs) x) of
    Error s -> [(c, s, t)] ++ findLine (ProgState vars funcs xs) (c + 1) t
    Result (newstate, Nothing) -> findLine newstate (c + 1) t
    Result (_, Just _) -> []

-- Make sure that the parameters have matching types for each parameter passed, otherwise would have unnasigned
funcTypeAlign :: String -> [String] -> [Type] -> [(Int, String, String)]
funcTypeAlign name [] [] = []
funcTypeAlign name (s : ss) [] =
  [(0, "Parameters passed have inconsistent matching to types", name)]
funcTypeAlign name [] (t : ts) =
  [(0, "Parameters passed have inconsistent matching to types", name)]
funcTypeAlign name (s : ss) (t : ts) = funcTypeAlign name ss ts

-- Use this to build a working state for functions to be checked for errors based on type
newFuncState :: State -> [String] -> [Type] -> State
newFuncState curState                     []       []       = curState
newFuncState curState                     []       (t : ts) = curState
newFuncState curState                     (s : ss) []       = curState
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

-- Use to locate type errors with the help of the type state checker
typeLine :: TypeState -> Int -> String -> [(Int, String, String)]
typeLine (ProgTypeState _ _ []) _ _ = []
typeLine (ProgTypeState types funcs (cmd : cmds)) ind fname =
  case (cmdType (ProgTypeState types funcs cmds) cmd) of
    Error s ->
      [(ind, "RunTime Data Type Error Found", fname)]
        ++ typeLine (ProgTypeState types funcs cmds) (ind + 1) fname
    Result (newstate, Error "") -> typeLine newstate (ind + 1) fname
    Result (_       , Result x) -> []

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
