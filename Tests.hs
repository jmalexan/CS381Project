module Doctest where

import           Language
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
    , Return (Element "myList" (Literal (Int 1)))
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
    [Set "myList" (Literal (IntList [0, 1, 2, 3])), Return (Length "myList")]

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
