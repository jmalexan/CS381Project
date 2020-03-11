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
