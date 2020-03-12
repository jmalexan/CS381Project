module TypeChecker where

import           Language
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Prelude                 hiding ( EQ
                                                , LT
                                                , List
                                                , and
                                                , or
                                                , subtract
                                                )

-- Maps variable names to the type of the variable.
type VarTypeAssociation = Map.Map String Type

data TypeState = ProgTypeState VarTypeAssociation FuncAssociation Prog
  deriving Show

-- Builds a new state object for use in a function call.  Takes arguments in this order: current program state, list of expr to fill args, list of arg names, empty var map (to be built), function definitions (to be passed), program block to execute
buildFuncTypeState
  :: TypeState
  -> [String]
  -> [Type]
  -> VarTypeAssociation
  -> FuncAssociation
  -> Prog
  -> Maybe TypeState
buildFuncTypeState _ [] [] vars funcs p = Just (ProgTypeState vars funcs p)
buildFuncTypeState _ [] _  vars funcs p = Nothing
buildFuncTypeState _ _  [] vars funcs p = Nothing
buildFuncTypeState oldstate (x : xs) (s : ss) vars funcs p =
  buildFuncTypeState oldstate xs ss (Map.insert x s vars) funcs p

-- Check the add operation types.
addType :: Type -> Type -> Maybe Type
addType TBool TBool = Nothing
addType a b | a == b    = Just a
            | otherwise = Nothing

-- Check the sub operation types.
subtractType :: Type -> Type -> Maybe Type
subtractType TBool TBool = Nothing
subtractType a b | a == b    = Just a
                 | otherwise = Nothing

-- Check the mul operation types.
multiplyType :: Type -> Type -> Maybe Type
multiplyType TBool TBool = Nothing
multiplyType a b | a == b    = Just a
                 | otherwise = Nothing

-- Check the div operation types.
divideType :: Type -> Type -> Maybe Type
divideType TBool TBool = Nothing
divideType a b | a == b    = Just a
               | otherwise = Nothing

-- Check the equal operation types.
equalType :: Type -> Type -> Maybe Type
equalType a b | a == b    = Just TBool
              | otherwise = Nothing

-- Check the less than operation types.
lessType :: Type -> Type -> Maybe Type
lessType TBool TBool = Nothing
lessType a b | a == b    = Just TBool
             | otherwise = Nothing

-- Check the and operation types.
andType :: Type -> Type -> Maybe Type
andType TBool TBool = Just TBool
andType _     _     = Nothing

-- Checks the types of an operation on two values.
operationType :: Operation -> Type -> Type -> Maybe Type
operationType Add   x y = addType x y
operationType Sub   x y = subtractType x y
operationType Mul   x y = multiplyType x y
operationType Div   x y = divideType x y
operationType Equal x y = equalType x y
operationType Less  x y = lessType x y
operationType And   x y = andType x y


checkFuncArgsType :: TypeState -> [Expr] -> [Type] -> Bool
checkFuncArgsType _ []       []       = True
checkFuncArgsType _ []       _        = False
checkFuncArgsType _ _        []       = False
checkFuncArgsType s (x : xs) (y : ys) = case exprType s x of
  Just z -> case z == y of
    True  -> checkFuncArgsType s xs ys
    False -> False
  _ -> False

exprType :: TypeState -> Expr -> Maybe Type
exprType oldstate (Operation oper expr1 expr2) =
  case (exprType oldstate expr1, exprType oldstate expr2) of
    (Just x, Just y) -> operationType oper x y
    _                -> Nothing
exprType oldstate (Not expr) = exprType oldstate expr
exprType (ProgTypeState vars _ _) (Variable name) = Map.lookup name vars
exprType _                        (Literal  (Int       _)) = Just TInt
exprType _                        (Literal  (Float     _)) = Just TFlt
exprType _                        (Literal  (Boolean   _)) = Just TBool
exprType _                        (Literal  (IntList   _)) = Just TIntList
exprType _                        (Literal  (FloatList _)) = Just TFltList
exprType _                        (Literal  (BoolList  _)) = Just TBoolList
exprType (ProgTypeState vars funcs p) (Element name index) =
  case (Map.lookup name vars, exprType (ProgTypeState vars funcs p) index) of
    (Just TIntList , Just TInt) -> Just TInt
    (Just TFltList , Just TInt) -> Just TFlt
    (Just TBoolList, Just TInt) -> Just TBool
    _                           -> Nothing
exprType (ProgTypeState vars _ _) (Length name) = case Map.lookup name vars of
  Just _ -> Just TInt
  _      -> Nothing
exprType oldstate (Concat l1 l2) =
  case (exprType oldstate l1, exprType oldstate l2) of
    (Just TIntList, Just TIntList) -> Just TIntList
    (Just TFltList, Just TFltList) -> Just TFltList
    (Just TBoolList, Just TBoolList) -> Just TBoolList
    _ -> Nothing
exprType (ProgTypeState vars funcs p) (Function name args) =
  case Map.lookup name funcs of
    Just (FuncDataCon argnames argtypes returntype fprog) ->
      case checkFuncArgsType (ProgTypeState vars funcs p) args argtypes of
        True  -> Just returntype
        False -> Nothing
    _ -> Nothing

cmdType :: TypeState -> Cmd -> Maybe (TypeState, Maybe Type)
cmdType (ProgTypeState vars funcs p) (Def name (FuncDataCon argnames argtypes returntype fp))
  = case
      buildFuncTypeState (ProgTypeState vars funcs p)
                         argnames
                         argtypes
                         Map.empty
                         funcs
                         fp
    of
      Just newts -> case progType newts of
        Just t
          | t == returntype -> Just
            ( ProgTypeState
              vars
              (Map.insert name
                          (FuncDataCon argnames argtypes returntype fp)
                          funcs
              )
              p
            , Nothing
            )
          | otherwise -> Nothing
        Nothing -> Nothing
      Nothing -> Nothing

cmdType (ProgTypeState vars funcs p) (Set name val) =
  case exprType (ProgTypeState vars funcs p) val of
    Just t -> case Map.lookup name vars of
      Just u -> case t == u of
        True  -> Just (ProgTypeState vars funcs p, Nothing)
        False -> Nothing
      Nothing -> Just (ProgTypeState (Map.insert name t vars) funcs p, Nothing)
    Nothing -> Nothing
cmdType (ProgTypeState vars funcs p) (Insert list index val) =
  case
      ( Map.lookup list vars
      , exprType (ProgTypeState vars funcs p) index
      , exprType (ProgTypeState vars funcs p) val
      )
    of
      (Just TIntList, Just TInt, Just TInt) ->
        Just (ProgTypeState vars funcs p, Nothing)
      (Just TFltList, Just TInt, Just TFlt) ->
        Just (ProgTypeState vars funcs p, Nothing)
      (Just TBoolList, Just TInt, Just TBool) ->
        Just (ProgTypeState vars funcs p, Nothing)
      _ -> Nothing
cmdType (ProgTypeState vars funcs p) (Delete list index) =
  case (Map.lookup list vars, exprType (ProgTypeState vars funcs p) index) of
    (Just TIntList , Just TInt) -> Just (ProgTypeState vars funcs p, Nothing)
    (Just TFltList , Just TInt) -> Just (ProgTypeState vars funcs p, Nothing)
    (Just TBoolList, Just TInt) -> Just (ProgTypeState vars funcs p, Nothing)
cmdType (ProgTypeState vars funcs p) (If condition block) = -- This case probably won't work, maybe prog is the wrong thing to call here.  possibly new function needed?  Issue here is that prog returns Error or a Type, and an if statement block doesn't necessarily return anything.
  case exprType (ProgTypeState vars funcs p) condition of
    Just TBool -> Just (ProgTypeState vars funcs (block ++ p), Nothing)
    Nothing    -> Nothing
cmdType (ProgTypeState vars funcs p) (While condition block) = -- Same as the above comment
  case exprType (ProgTypeState vars funcs p) condition of
    Just TBool -> Just (ProgTypeState vars funcs (block ++ p), Nothing)
    Nothing    -> Nothing
cmdType (ProgTypeState vars funcs p) (ForEach item list block) =
  case exprType (ProgTypeState vars funcs p) list of
    Just TIntList -> Just
      (ProgTypeState (Map.insert item TInt vars) funcs (block ++ p), Nothing)
    Just TFltList -> Just
      (ProgTypeState (Map.insert item TFlt vars) funcs (block ++ p), Nothing)
    Just TBoolList -> Just
      (ProgTypeState (Map.insert item TBool vars) funcs (block ++ p), Nothing)
    _ -> Nothing
cmdType (ProgTypeState vars funcs p) (Return expr1) =
  case exprType (ProgTypeState vars funcs p) expr1 of
    Just t  -> Just (ProgTypeState vars funcs p, Just t)
    Nothing -> Nothing


progType :: TypeState -> Maybe Type
progType (ProgTypeState _ _ []) = Just TBool --base case
progType (ProgTypeState vars funcs (x : xs)) =
  case cmdType (ProgTypeState vars funcs xs) x of
    Just (newstate, Nothing) -> progType newstate
    Just (_       , Just x ) -> Just x
    Nothing                  -> Nothing

typecheck :: Prog -> Maybe Type
typecheck p = progType (ProgTypeState Map.empty Map.empty (prelude ++ p)) -- Adds prelude functions

compile :: Prog -> String
compile p =
  case
      ((findLine (ProgState Map.empty Map.empty (prelude ++ p)) (-1) ("Main")) ++ (typeLine (ProgTypeState Map.empty Map.empty (prelude ++ p) ) (-1) ("Main")))
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
        (concatenator(
          (findLine (ProgState Map.empty Map.empty (prelude ++ p) ) (-1) ("Main"))
          ++ (typeLine (ProgTypeState Map.empty Map.empty (prelude ++ p) ) (-1) ("Main")))
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


funcTypeAlign :: String -> [String] -> [Type] -> [(Int, String, String)]
funcTypeAlign name [] [] = []
funcTypeAlign name (s:ss) [] = [(0, "Parameters passed have inconsistent matching to types", name)]
funcTypeAlign name [] (t:ts) = [(0, "Parameters passed have inconsistent matching to types", name)]
funcTypeAlign name (s:ss) (t:ts) = funcTypeAlign name ss ts

-- Use this to build a working state for functions to be checked for errors based on type
newFuncState :: State -> [String] -> [Type] -> State
newFuncState curState                     []       []       = curState
newFuncState curState [] (t:ts) = curState
newFuncState curState (s:ss) [] = curState
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

typeLine :: TypeState -> Int -> String -> [(Int, String, String)]
typeLine (ProgTypeState types funcs (cmd:cmds)) ind fname = case (cmdType (ProgTypeState types funcs cmds) cmd) of
  Nothing -> [(ind, "Data Type Error Found", fname)] ++ typeLine (ProgTypeState types funcs cmds) (ind + 1) fname
  Just (newstate, Nothing) -> typeLine newstate (ind + 1) fname
  Just (_       , Just x ) -> []

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
