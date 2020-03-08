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
    , Index "myList" (Literal (Int 0)) (Literal (Int 0))
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

