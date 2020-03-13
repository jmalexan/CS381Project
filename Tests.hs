module Doctest where

import           CoreLanguage -- Allows writing programs
import           Interpreter -- Allows running programs `compile <name>`

import           Prelude                 hiding ( and
                                                , or
                                                , subtract
                                                )

-- | Testing the `or` syntactic sugar
--
--   >>> testOrFalse
--   Result (Boolean False)
--
testOrFalse :: MaybeError VarVal
testOrFalse =
    run [Return (or (Literal (Boolean False)) (Literal (Boolean False)))]
-- |
--   >>> testOrTrue
--   Result (Boolean True)
--
testOrTrue :: MaybeError VarVal
testOrTrue =
    run [Return (or (Literal (Boolean False)) (Literal (Boolean True)))]

-- | Testing the list assignment
--
--   >>> testListAssignment
--   Result (IntList [1,2,3])
--
testListAssignment :: MaybeError VarVal
testListAssignment =
    run [Set "myList" (Literal (IntList [1, 2, 3])), Return (Variable "myList")]

-- | Testing the list insertion
--
--   >>> testListInsertion
--   Result (IntList [0,1,2,3])
--
testListInsertion :: MaybeError VarVal
testListInsertion = run
    [ Set "myList" (Literal (IntList [1, 2, 3]))
    , Insert "myList" (Literal (Int 0)) (Literal (Int 0))
    , Return (Variable "myList")
    ]

-- | Testing the list accessing
--
--   >>> testListAccess
--   Result (Int 2)
--
testListAccess :: MaybeError VarVal
testListAccess = run
    [ Set "myList" (Literal (IntList [1, 2, 3]))
    , Return (Element (Variable "myList") (Literal (Int 1)))
    ]

-- | Testing the list deletion
--
--   >>> testListDeletion
--   Result (IntList [0,1,3])
--
testListDeletion :: MaybeError VarVal
testListDeletion = run
    [ Set "myList" (Literal (IntList [0, 1, 2, 3]))
    , Delete "myList" (Literal (Int 2))
    , Return (Variable "myList")
    ]

-- | Testing the list index assignment
--
--   >>> testListIndexing
--   Result (IntList [0,8,2,3])
--
testListIndexing :: MaybeError VarVal
testListIndexing = run
    [ Set "myList" (Literal (IntList [0, 1, 2, 3]))
    , assign "myList" (Literal (Int 1)) (Literal (Int 8))
    , Return (Variable "myList")
    ]

-- | Testing list length
--
--   >>> testListLength
--   Result (Int 4)
--
testListLength :: MaybeError VarVal
testListLength = run
    [ Set "myList" (Literal (IntList [0, 1, 2, 3]))
    , Return (Length (Variable "myList"))
    ]

-- | Testing built-in append
--
--   >>> testListLength
--   Result (Int 4)
--
testListAppend :: MaybeError VarVal
testListAppend = run
    [ Set "myList" (Literal (IntList [0, 1, 2, 3]))
    , Return (Function "append" [Variable "myList", Literal (Int 4)])
    ]

-- | Testing built-in range
--
--   >>> testRange
--   Result (IntList [0,1,2,3])
--
testRange :: MaybeError VarVal
testRange = run [Return (Function "range" [Literal (Int 4)])]


-- | Testing the string assignment
--
--   >>> testStringAssignment
--   Result (String "hello")
--
testStringAssignment :: MaybeError VarVal
testStringAssignment = run
    [ Set "myString"    (Literal (String "hello"))
    , Set "otherString" (Variable "myString")
    , Return (Variable "myString")
    ]

-- | Testing the string insertion
--
--   >>> testStringInsertion
--   Result (String "Hello, world")
--
testStringInsertion :: MaybeError VarVal
testStringInsertion = run
    [ Set "myString" (Literal (String "ello, world"))
    , Insert "myString" (Literal (Int 0)) (Literal (Character 'H'))
    , Return (Variable "myString")
    ]

-- | Testing the string accessing
--
--   >>> testStringAccess
--   Result (Character 'e')
--
testStringAccess :: MaybeError VarVal
testStringAccess = run
    [ Set "myString" (Literal (String "Hello, world"))
    , Return (Element (Variable "myString") (Literal (Int 1)))
    ]

-- | Testing the string deletion
--
--   >>> testStringDeletion
--   Result (String "Hello, world")
--
testStringDeletion :: MaybeError VarVal
testStringDeletion = run
    [ Set "myString" (Literal (String "Hello, woorld"))
    , Delete "myString" (Literal (Int 8))
    , Return (Variable "myString")
    ]

-- | Testing the list index assignment
--
--   >>> testStringIndexing
--   Result (String "Hello, world")
--
testStringIndexing :: MaybeError VarVal
testStringIndexing = run
    [ Set "myString" (Literal (String "Hello, w0rld"))
    , assign "myString" (Literal (Int 8)) (Literal (Character 'o'))
    , Return (Variable "myString")
    ]

-- | Testing list length
--
--   >>> testStringLength
--   Result (Int 12)
--
testStringLength :: MaybeError VarVal
testStringLength = run
    [ Set "myString" (Literal (String "Hello, world"))
    , Return (Length (Variable "myString"))
    ]

-- | Testing built-in append
--
--   >>> testStringAppend
--   Result (String "Hello, world!")
--
testStringAppend :: MaybeError VarVal
testStringAppend = run
    [ Set "myString" (Literal (String "Hello, world"))
    , Return (Function "append" [Variable "myString", Literal (Character '!')])
    ]

-- | Testing string concatenation
--
--   >>> testStringConcat
--   Result (String "You're a wizard, Harry")
--
testStringConcat :: MaybeError VarVal
testStringConcat = run
    [ Set "myString" (Literal (String "You're a wizard, "))
    , Return (Concat (Variable "myString") (Literal (String "Harry")))
    ]

-- | Tests casting string to int
--
--   >>> testCasting
--   Result (Int 12382)
--
testCasting :: MaybeError VarVal
testCasting = run
    [ Set "myString" (Cast (Literal (String "12382")) TInt)
    , Return (Variable "myString")
    ]
