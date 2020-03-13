module TypeChecker where
-- Description: Typechecks programs in our language (static type checking).
-- Authors:
--  > Faaiq Waqar (waqarf)
--  > Jonathan Alexander (alexajon)
--  > Julian Fortune (fortunej)

import           CoreLanguage

import qualified Data.Map.Strict               as Map
import qualified Data.List                     as List
import           Prelude                 hiding ( and
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
  -> MaybeError TypeState
buildFuncTypeState _ [] [] vars funcs p = Result (ProgTypeState vars funcs p)
buildFuncTypeState _ [] _  vars funcs p = Error ""
buildFuncTypeState _ _  [] vars funcs p = Error ""
buildFuncTypeState oldstate (x : xs) (s : ss) vars funcs p =
  buildFuncTypeState oldstate xs ss (Map.insert x s vars) funcs p

-- Check the add operation types.
numericOperationType :: String -> Type -> Type -> MaybeError Type
numericOperationType _ TInt TInt = Result TInt
numericOperationType _ TFlt TFlt = Result TFlt
numericOperationType name _ _ =
  Error (name ++ " can only be applied to Ints or Floats.")

-- Check the equal operation types.
equalType :: Type -> Type -> MaybeError Type
equalType a b | a == b    = Result TBool
              | otherwise = Error ""

-- Check the less than operation types.
lessType :: Type -> Type -> MaybeError Type
lessType TBool TBool = Error ""
lessType a b | a == b    = Result TBool
             | otherwise = Error ""

-- Check the and operation types.
andType :: Type -> Type -> MaybeError Type
andType TBool TBool = Result TBool
andType _     _     = Error ""

-- Checks the types of an operation on two values.
operationType :: Operation -> Type -> Type -> MaybeError Type
operationType Add   x y = numericOperationType "Addition" x y
operationType Sub   x y = numericOperationType "Subtraction" x y
operationType Mul   x y = numericOperationType "Multiplication" x y
operationType Div   x y = numericOperationType "Division" x y
operationType Equal x y = equalType x y
operationType Less  x y = lessType x y
operationType And   x y = andType x y


checkFuncArgsType :: TypeState -> [Expr] -> [Type] -> Bool
checkFuncArgsType _ []       []       = True
checkFuncArgsType _ []       _        = False
checkFuncArgsType _ _        []       = False
checkFuncArgsType s (x : xs) (y : ys) = case exprType s x of
  Result z -> case z == y of
    True  -> checkFuncArgsType s xs ys
    False -> False
  _ -> False

legalCastMap :: Map.Map Type [Type]
legalCastMap = Map.fromList
  [ (TInt   , [TInt, TFlt, TChar, TString])
  , (TFlt   , [TInt, TFlt, TString])
  , (TChar  , [TInt, TChar, TString])
  , (TString, [TInt, TFlt, TBool, TString])
  ]

castType :: Type -> Type -> MaybeError Type
castType a b = case Map.lookup a legalCastMap of
  Just legalCasts -> case List.elemIndex b legalCasts of
    Just _  -> Result b
    Nothing -> Error ("Cannot cast " ++ show a ++ " to " ++ show b)
  Nothing -> Error ("Cannot cast " ++ show a ++ " to any types")

exprType :: TypeState -> Expr -> MaybeError Type
exprType oldstate (Operation oper expr1 expr2) =
  case (exprType oldstate expr1, exprType oldstate expr2) of
    (Result x, Result y) -> operationType oper x y
    (Error  s, _       ) -> Error s
    (_       , Error s ) -> Error s
exprType oldstate (Not expr) = exprType oldstate expr
exprType (ProgTypeState vars _ _) (Variable name) =
  case Map.lookup name vars of
    Just a  -> Result a
    Nothing -> Error ("Variable '" ++ name ++ "' reference before assignment.")
exprType _ (Literal (Int       _)) = Result TInt
exprType _ (Literal (Float     _)) = Result TFlt
exprType _ (Literal (Boolean   _)) = Result TBool
exprType _ (Literal (IntList   _)) = Result TIntList
exprType _ (Literal (FloatList _)) = Result TFltList
exprType _ (Literal (BoolList  _)) = Result TBoolList
exprType _ (Literal (String    _)) = Result TString
exprType oldState (Element list index) =
  case (exprType oldState list, exprType oldState index) of
    (Result TIntList , Result TInt) -> Result TInt
    (Result TFltList , Result TInt) -> Result TFlt
    (Result TBoolList, Result TInt) -> Result TBool
    (Result TString  , Result TInt) -> Result TChar
    (Result _, Result TInt) -> Error "Cannot get the element of a non-list type"
    (_, Result _) -> Error "Index not an integer type"
    (Error s, _)                               -> Error s
    (_, Error s) -> Error s
exprType oldState (Length list) = case exprType oldState list of
  Result x -> Result TInt
  Error s        -> Error s
exprType oldstate (Concat l1 l2) =
  case (exprType oldstate l1, exprType oldstate l2) of
    (Result TIntList, Result TIntList) -> Result TIntList
    (Result TFltList, Result TFltList) -> Result TFltList
    (Result TBoolList, Result TBoolList) -> Result TBoolList
    (Result TString, Result TString) -> Result TString
    (Result _, _) -> Error "First expression in concat not list type"
    (_, Result _) -> Error "Second expression in concat not list type"
    (Error s, _) -> Error s
    (_, Error s) -> Error s
exprType oldstate (Cast expr newType) = case exprType oldstate expr of
  Result x -> castType x newType
  Error  s -> Error s
exprType (ProgTypeState vars funcs p) (Function name args) =
  case Map.lookup name funcs of
    Just (FuncDataCon argnames argtypes returntype fprog) ->
      case checkFuncArgsType (ProgTypeState vars funcs p) args argtypes of
        True  -> Result returntype
        False -> Error "Invalid argument types for function call"
    _ -> Error "No such function"

cmdType :: TypeState -> Cmd -> MaybeError (TypeState, Maybe Type)
cmdType (ProgTypeState vars funcs p) (Def name (FuncDataCon argnames argtypes returntype fp))
  = case
      buildFuncTypeState (ProgTypeState vars funcs p)
                         argnames
                         argtypes
                         Map.empty
                         funcs
                         fp
    of
      Result newts -> case progType newts of
        Result t
          | t == returntype -> Result
            ( ProgTypeState
              vars
              (Map.insert name
                          (FuncDataCon argnames argtypes returntype fp)
                          funcs
              )
              p
            , Nothing
            )
          | otherwise -> Error "Function does not return declared return type"
        Error s -> Error s
      Error s -> Error s

cmdType (ProgTypeState vars funcs p) (Set name val) =
  case exprType (ProgTypeState vars funcs p) val of
    Result t -> case Map.lookup name vars of
      Just u
        | t == u -> Result (ProgTypeState vars funcs p, Nothing)
        | otherwise -> Error "Invalid type assignemnt"
      Nothing ->
        Result (ProgTypeState (Map.insert name t vars) funcs p, Nothing)
    Error s -> Error s
cmdType (ProgTypeState vars funcs p) (Insert list index val) =
  case
      ( Map.lookup list vars
      , exprType (ProgTypeState vars funcs p) index
      , exprType (ProgTypeState vars funcs p) val
      )
    of
      (Just TIntList, Result TInt, Result TInt) ->
        Result (ProgTypeState vars funcs p, Nothing)
      (Just TFltList, Result TInt, Result TFlt) ->
        Result (ProgTypeState vars funcs p, Nothing)
      (Just TBoolList, Result TInt, Result TBool) ->
        Result (ProgTypeState vars funcs p, Nothing)
      (Just TString, Result TInt, Result TChar) ->
        Result (ProgTypeState vars funcs p, Nothing)
      (Just _, Result TInt, Result _) -> Error "Invalid list or value in insert function"
      (Just _, Result _, Result _) -> Error "Index not an integer type"
cmdType (ProgTypeState vars funcs p) (Delete list index) =
  case (Map.lookup list vars, exprType (ProgTypeState vars funcs p) index) of
    (Just TIntList, Result TInt) ->
      Result (ProgTypeState vars funcs p, Nothing)
    (Just TFltList, Result TInt) ->
      Result (ProgTypeState vars funcs p, Nothing)
    (Just TBoolList, Result TInt) ->
      Result (ProgTypeState vars funcs p, Nothing)
    (Just TString, Result TInt) -> Result (ProgTypeState vars funcs p, Nothing)
    (Nothing     , _          ) -> Error "Variable not found"
    (_           , _          ) -> Error "Type mistmatch"
cmdType (ProgTypeState vars funcs p) (If condition block) = -- This case probably won't work, maybe prog is the wrong thing to call here.  possibly new function needed?  Issue here is that prog returns Error or a Type, and an if statement block doesn't necessarily return anything.
  case exprType (ProgTypeState vars funcs p) condition of
    Result TBool -> Result (ProgTypeState vars funcs (block ++ p), Nothing)
    Error  s    -> Error s
cmdType (ProgTypeState vars funcs p) (While condition block) = -- Same as the above comment
  case exprType (ProgTypeState vars funcs p) condition of
    Result TBool -> Result (ProgTypeState vars funcs (block ++ p), Nothing)
    Error  s    -> Error s
cmdType (ProgTypeState vars funcs p) (ForEach item list block) =
  case exprType (ProgTypeState vars funcs p) list of
    Result TIntList -> Result
      (ProgTypeState (Map.insert item TInt vars) funcs (block ++ p), Nothing)
    Result TFltList -> Result
      (ProgTypeState (Map.insert item TFlt vars) funcs (block ++ p), Nothing)
    Result TBoolList -> Result
      (ProgTypeState (Map.insert item TBool vars) funcs (block ++ p), Nothing)
    Result TString -> Result
      (ProgTypeState (Map.insert item TChar vars) funcs (block ++ p), Nothing)
    _ -> Error "Cannot iterate over non list type"
cmdType (ProgTypeState vars funcs p) (Return expr1) =
  case exprType (ProgTypeState vars funcs p) expr1 of
    Result t  -> Result (ProgTypeState vars funcs p, Just t)
    Error  s -> Error s


progType :: TypeState -> MaybeError Type
progType (ProgTypeState _ _ []) = Result TBool --base case
progType (ProgTypeState vars funcs (x : xs)) =
  case cmdType (ProgTypeState vars funcs xs) x of
    Result (newstate, Nothing) -> progType newstate
    Result (_       , Just x ) -> Result x
    Error  s                   -> Error s

typecheck :: Prog -> MaybeError Type
typecheck p = progType (ProgTypeState Map.empty Map.empty p) -- Adds prelude functions
