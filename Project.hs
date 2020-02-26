module Project where
-- Description: Final Project for CS 381 Winter 2020
-- Authors:
--  > Faaiq Waqar (onid)
--  > Jonathan Alexander (onid)
--  > Julian Fortune (fortunej)

import qualified Data.HashMap.Strict as Map
-- Library for associating keys with values.
--
-- I believe this is the best version for us because:
--  > It is designed for use with string keys
--  > It appears to perform the best
--  > We `don't care about ordering`
--  > Strict is `more efficient when laziness is not necessary`
--
-- Note: This module has some naming conflicts with Prelude (thus `qualified`)
-- Definition: `data HashMap k v = Empty | ... `
-- Source code: https://hackage.haskell.org/package/unordered-containers-0.2.10.0/docs/src/Data.HashMap.Base.html#HashMap

-- | Julian's experiments with HashMap

-- Holds different literal/storable types that could be in a variable
data Value = Int Integer | Str String | Flt Float
    deriving Show

-- Maps a string (variable name) to a `Value`
type Association = Map.HashMap String Value

exampleHashMap :: Association
exampleHashMap =  ( Map.insert "c" (Flt 3.14159) .
                    Map.insert "b" (Str "hello, world!") .
                    Map.insert "a" (Int 6) ) Map.empty

exampleLookup :: Maybe Value
exampleLookup =  Map.lookup "b" exampleHashMap