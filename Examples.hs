module Examples where

import           Language
import qualified Data.Map.Strict               as Map
import           Data.Maybe

mainProg :: Prog
mainProg =
    [ Set "x" (ExprVal (Int 3))
    , Def
        "test"
        (FuncDataCon
            ["asdf"]
            [ Set "b" (exprSum (ExprVal (Int 777)) (ExprVar "asdf"))
            , Set "b" (exprDiv (ExprVar "b") (ExprVal (Int 2)))
            , Return (ExprVar "b")
            ]
        )
    , Set "x" (ExprFunc "test" [ExprVar "x"])
    , If (exprDiv (ExprVar "x") (ExprVal (Int 381)))
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
            [ Set "b" (exprSum (ExprVal (Int 777)) (ExprVar "asdf"))
            , Set "b" (exprDiv (ExprVar "b") (ExprVal (Flt 2)))
            , Return (ExprVar "b")
            ]
        )
    , Set "x" (ExprFunc "test" [ExprVar "x"])
    , If (exprEQ (ExprVar "x") (ExprVal (Int 381)))
         [Set "x" (ExprVal (Boolean True))]
    , Return (ExprVar "x")
    ]

badState :: State
badState = ProgState Map.empty Map.empty badProg

test1Prog :: Prog
test1Prog = [Return (ExprNumOp Div (ExprVal (Int 5)) (ExprVal (Int 2)))]

test1State :: State
test1State = ProgState Map.empty Map.empty test1Prog
