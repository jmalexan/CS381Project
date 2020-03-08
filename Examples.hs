module Examples where

import           Language
import           Prelude                 hiding ( and
                                                , or
                                                , subtract
                                                )

-- An example of a valid program. Run using `run goodProg`
goodProg :: Prog
goodProg =
  [ Set "x" (Literal (Int 3))
  , Def
    "test"
    (FuncDataCon
      ["asdf"]
      [ Set "b" (Operation Add (Literal (Int 777)) (Variable "asdf"))
      , Set "b" (Operation Div (Variable "b") (Literal (Int 2)))
      , Return (Variable "b")
      ]
    )
  , Set "x" (Function "test" [Variable "x"])
  , If (Operation Equal (Variable "x") (Literal (Int 381)))
       [Set "x" (Literal (Boolean True))]
  , Return (Variable "x")
  ]

-- An example of an invalid program. Run using `run badProg`
badProg :: Prog
badProg =
  [ Set "x" (Literal (Int 3))
  , Def
    "test"
    (FuncDataCon
      ["asdf"]
      [ Set "b" (Operation Add (Literal (Int 777)) (Variable "asdf"))
      , Set "b" (Operation Div (Variable "b") (Literal (Flt 2)))
      , Return (Variable "b")
      ]
    )
  , Set "x" (Function "test" [Variable "x"])
  , If (Operation Equal (Variable "x") (Literal (Int 381)))
       [Set "x" (Literal (Boolean True))]
  , Return (Variable "x")
  ]

test1Prog :: Prog
test1Prog = [Return (Operation Div (Literal (Int 5)) (Literal (Int 2)))]

testOr1 :: Prog
testOr1 = [Return (or (Literal (Boolean True)) (Literal (Boolean False)))]

testOr2 :: Prog
testOr2 = [Return (or (Literal (Boolean False)) (Literal (Boolean False)))]

testListAppend :: Prog
testListAppend =
  [ Set "myList" (Literal (IntList [1, 2, 3]))
  , Index "myList" (Literal (Int 0)) (Literal (Int 0))
  , Return (Variable "myList")
  ]
