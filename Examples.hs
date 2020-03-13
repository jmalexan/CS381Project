module Examples where
-- Description: Example programs in our language.
-- Authors:
--  > Faaiq Waqar (waqarf)
--  > Jonathan Alexander (alexajon)
--  > Julian Fortune (fortunej)

import           CoreLanguage -- Allows writing programs
import           Interpreter -- Allows running programs `compile <name>`
import           TypeChecker -- Allows running programs `typecheck <name>`
import           Prelude                 hiding ( and
                                                , or
                                                , subtract
                                                )

badFuncNoExist :: Prog
badFuncNoExist = [Return (Function "test" [])]

badOperTypes :: Prog
badOperTypes = [Return (Operation Add (Literal (Int 3)) (Literal (Boolean False)))]

badCastType :: Prog
badCastType = [Return (Cast (Literal (Int 3)) TIntList)]

badCastValue :: Prog
badCastValue = [Return (Cast (Literal (String "f")) TBool)]

badVarRef :: Prog
badVarRef = [Return (Variable "test")]

badElement :: Prog
badElement = [Return (Element (Literal (IntList [3, 6, 9])) (Literal (Boolean False)))]

badFuncRef :: Prog
badFuncRef = [Return (Function "test" [])]

badFuncRedeclare :: Prog
badFuncRedeclare =
  [ Def "test" (FuncDataCon [] [] TInt [Return (Literal (Int 3))])
  , Def "test" (FuncDataCon [] [] TInt [Return (Literal (Int 4))])
  ]

badFuncArgs :: Prog
badFuncArgs =
  [ Def "test" (FuncDataCon [] [] TInt [Return (Literal (Int 3))])
  , Return (Function "test" [(Literal (Int 3))])]

badIfCondition :: Prog
badIfCondition = [If (Literal (Int 3)) [Return (Literal (Boolean True))]]

badWhileCondition :: Prog
badWhileCondition = [While (Literal (Int 3)) [Return (Literal (Boolean True))]]

badForEachIterator :: Prog
badForEachIterator = [ForEach "test" (Literal (Int 3)) [Return (Literal (Boolean True))]]

badInsertIndex :: Prog
badInsertIndex =
  [ Set "list" (Literal (IntList [3, 6, 9]))
  , Insert "list" (Literal (Boolean False)) (Literal (Int 4))
  ]

badDeleteIndex :: Prog
badDeleteIndex =
  [ Set "list" (Literal (IntList [3, 6, 9]))
  , Delete "list" (Literal (Boolean False))
  ]

badVarRedeclaration :: Prog
badVarRedeclaration =
  [ Set "test" (Literal (Int 4))
  , Set "test" (Literal (Boolean False))]

badFuncArgTypes :: Prog
badFuncArgTypes =
  [ Def "test" (FuncDataCon ["thing"] [TInt] TInt [Return (Variable "thing")])
  , Return (Function "test" [(Literal (Boolean False))])]

badFuncReturnType :: Prog
badFuncReturnType = 
  [ Def "test" (FuncDataCon [] [] TBool [Return (Literal (Int 3))])]

-- An example of a valid program. Run using `run goodProg`
goodProg :: Prog
goodProg =
  [ Set "x" (Literal (Int 3))
  , Def
    "test"
    (FuncDataCon
      ["asdf"]
      [TInt]
      TInt
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
      [TInt]
      TInt
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
      ["input"]
      [TInt]
      TInt
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

-- An implementation of quicksort in Barely Functional-C. Run with `compile quicksort`
quickSort :: Prog
quickSort =
  [ Def
    "quickSort"
    (FuncDataCon
      ["list"]
      [TIntList]
      TIntList
      [ If (Operation Equal (Length (Variable "list")) (Literal (Int 0))) -- Check base cases
           [Return (Variable "list")]
      , If
        (greater (Length (Variable "list")) (Literal (Int 0)))
        [ Set "pivot"
              (Operation Div (Length (Variable "list")) (Literal (Int 2))) -- Pick pivot as middle (uses integer division so pivot is int)
        , Set "left"  (Literal (IntList []))
        , Set "right" (Literal (IntList []))
        , ForEach
          "x"
          (Variable "list")
          [ If
            (Operation Less
                       (Variable "x")
                       (Element (Variable "list") (Variable "pivot"))
            ) -- x < input[pivot]
            [Set "left" (Function "append" [Variable "left", Variable "x"])]
          , If
            (greater (Variable "x")
                     (Element (Variable "list") (Variable "pivot"))
            ) -- x >= input[pivot]
            [Set "right" (Function "append" [Variable "right", Variable "x"])]
          ]
        , Set "sortedLeft"  (Function "quickSort" [Variable "left"])
        , Set "sortedRight" (Function "quickSort" [Variable "right"])
        , Return
          (Concat
            (Function
              "append"
              [ Variable "sortedLeft"
              , Element (Variable "list") (Variable "pivot")
              ]
            )
            (Variable "sortedRight")
          ) -- append(sortedLeft, pivot]) ++ sortedRight
        ]
      ]
    )
  , Return
    (Function "quickSort" [Literal (IntList [9, 5, 4, 8, 3, 7, 2, 6, 1])]) -- Modify these values to change test different inputs
  ]
