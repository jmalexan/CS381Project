module Examples where

import           Language
import           TypeChecker
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
      ["asdf"] [TInt] TInt
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
      ["asdf"] [TInt] TInt
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

-- An example program to show off the ability of lists by calculating factorial.
factorial :: Prog
factorial =
  [ Def
    "factorial"
    (FuncDataCon
      ["input"] [TInt] TInt
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
  , Return (Function "factorial" [Literal (Int 6)]) -- Modify this value to change test different inputs
  ]

-- An implementation of quicksort in Barely Functional-C. Run with `run quicksort`
quickSort :: Prog
quickSort =
  [ Def
    "quickSort"
    (FuncDataCon
      ["list"] [TIntList] TIntList
      [ If (Operation Equal (Length "list") (Literal (Int 0))) -- Check base cases
           [Return (Variable "list")]
      , If
        (greater (Length "list") (Literal (Int 0)))
        [ Set "pivot" (Operation Div (Length "list") (Literal (Int 2))) -- Pick pivot as middle (uses integer division so pivot is int)
        , Set "left"  (Literal (IntList []))
        , Set "right" (Literal (IntList []))
        , ForEach
          "x"
          (Variable "list")
          [ If
            (Operation Less (Variable "x") (Element "list" (Variable "pivot"))) -- x < input[pivot]
            [Set "left" (Function "append" [Variable "left", Variable "x"])]
          , If
            (greater (Variable "x") (Element "list" (Variable "pivot"))) -- x >= input[pivot]
            [Set "right" (Function "append" [Variable "right", Variable "x"])]
          ]
        , Set "sortedLeft"  (Function "quickSort" [Variable "left"])
        , Set "sortedRight" (Function "quickSort" [Variable "right"])
        , Return
          (Concat
            (Function
              "append"
              [Variable "sortedLeft", Element "list" (Variable "pivot")]
            )
            (Variable "sortedRight")
          ) -- append(sortedLeft, pivot]) ++ sortedRight
        ]
      ]
    )
  , Return
    (Function "quickSort" [Literal (IntList [9, 5, 4, 8, 3, 7, 2, 6, 1])]) -- Modify these values to change test different inputs
  ]
