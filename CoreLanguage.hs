module CoreLanguage where
-- Description: The core of the language. Includes the abstract grammars and foundational types.
-- Authors:
--  > Faaiq Waqar (waqarf)
--  > Jonathan Alexander (alexajon)
--  > Julian Fortune (fortunej)

import qualified Data.Map.Strict               as Map
import           Prelude                 hiding ( and
                                                , or
                                                , subtract
                                                )

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

-- All built in types in the language.
data Type = TInt
          | TFlt
          | TBool
          | TChar
          | TIntList
          | TFltList
          | TBoolList
          | TString
    deriving (Show, Prelude.Eq, Ord)

-- ??
data CompVal = Loaded | Syntaxerror | Datatypeerror
    deriving Show

-- Maps variable names to the stored value.
type VarAssociation = Map.Map String VarVal

-- The data associated with a function.
data FuncData = FuncDataCon [String] [Type] Type Prog
  deriving Show

-- Maps function names to the desired function.
type FuncAssociation = Map.Map String FuncData

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
          | Cast Expr Type-- Cast from a VarVal of one type to another (specified by Type).
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
