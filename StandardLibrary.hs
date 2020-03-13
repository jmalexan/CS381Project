module StandardLibrary where
-- Description: Built-in functions automatically imported to all interpreted programs (prelude).
-- Authors:
--  > Faaiq Waqar (waqarf)
--  > Jonathan Alexander (alexajon)
--  > Julian Fortune (fortunej)

import           CoreLanguage
import           Prelude                 hiding ( and
                                                , or
                                                , subtract
                                                )

--------------------------------------------------------------
-- Prelude (Built-in Library)
--------------------------------------------------------------

prelude :: Prog
prelude =
    [ Def -- Append: Takes list and element and returns list with element appended. (This will break when typeSystem is implemented)
        "append"
        (FuncDataCon
            ["list", "element"]
            [TIntList, TInt]
            TIntList
            [ Set "index" (Length (Variable "list")) -- Get length of list
            , Insert "list" (Variable "index") (Variable "element")
            , Return (Variable "list")
            ]
        )
    , Def -- Range: Takes and int and generates Int list [0...input].
        "range"
        (FuncDataCon
            ["count"]
            [TInt]
            TIntList
            [ Set "index" (Literal (Int 0)) -- Get length of list
            , Set "list"  (Literal (IntList []))
            , While
                (Operation Less (Variable "index") (Variable "count"))
                [ Set
                    "list"
                    (Function "append" [Variable "list", Variable "index"]) -- Append
                , Set "index"
                      (Operation Add (Variable "index") (Literal (Int 1))) -- Increment index
                ]
            , Return (Variable "list")
            ]
        )
    ]
