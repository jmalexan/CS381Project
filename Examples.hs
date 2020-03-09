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
      , Set "b" (Operation Div (Variable "b") (Literal (Float 2)))
      , Return (Variable "b")
      ]
    )
  , Set "x" (Function "test" [Variable "x"])
  , If (Operation Equal (Variable "x") (Literal (Int 381)))
       [Set "x" (Literal (Boolean True))]
  , Return (Variable "x")
  ]

-- Shows off the for each functionality
sumListProg :: Prog
sumListProg =
  [ Set "x" (Literal (IntList [1, 2, 3])) -- Modify this lest to test different inputs.
  , Def
    "sumList"
    (FuncDataCon
      ["inputList"]
      [ Set "sum" (Literal (Int 0))
      , ForEach "x"
                (Variable "inputList")
                [Set "sum" (Operation Add (Variable "sum") (Variable "x"))]
      , Return (Variable "sum")
      ]
    )
  , Return (Function "sumList" [Variable "x"])
  ]

-- An example program to show off the ability of lists by calculating factorial.
factorial :: Prog
factorial =
  [ Def
    "factorial"
    (FuncDataCon
      ["input"]
      [ Set "factorial" (Literal (Int 1))
      , ForEach
        "x"
        (Function "range" [Variable "input"])
        [ Set
            "factorial"
            (Operation Mul
                       (Variable "factorial")
                       (Operation Add (Variable "x") (Literal (Int 1)))
            )
        ]
      , Return (Variable "factorial")
      ]
    )
  , Return (Function "factorial" [Literal (Int 6)])
  ]
