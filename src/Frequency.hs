{-# LANGUAGE BangPatterns #-}

-------------------------------------------------------------------------
--                              
--         Frequency.hs
--         U6161816
--         Calculate the frequencies of words in a text, used in
--         Huffman coding.
-------------------------------------------------------------------------
module Frequency
  ( frequency
  ) where

import Prelude hiding (lookup)
import Data.Map (Map, lookup, insert, empty, assocs)

-- A frequency histogram is a map from values to `Int`s
type Histogram a = Map a Int


-- | inc
-- Increment the value associated with the given key in the input histogram.
-- WARNING: Do not change this function!
-- Example:
-- >>> inc empty 'a'
-- fromList [('a',1)]
inc
  :: Ord a
  => Histogram a -> a -> Histogram a
inc !h x =
  case lookup x h of
    Nothing -> insert x 1 h
    Just !n -> insert x (n + 1) h
-- The use of exclamation sign: http://stackoverflow.com/questions/993112/what-does-the-exclamation-mark-mean-in-a-haskell-declaration

-- Calculate the frequencies of characters in a list.
frequency
  :: Ord a
  => [a] -> [(a, Int)]
frequency xs = assocs (create empty xs)

-- | create
-- Create a new histogram that augments the input histogram with
-- the keys given in the input list.
--
-- Example:
-- >>> create empty "abacab"
-- fromList [('a',3),('b',2),('c',1)]
create
  :: Ord a
  => Histogram a -> [a] -> Histogram a
create h [] = h
create h (x:xs) = inc (create h xs) x
--Understanding of inc: to some extent, it is like the list constructor, ":".

--To test whether the function "frequency" gives correct result for a given value
test1 :: Bool
test1 = frequency "aaabbcddddeeeeeee" == [('a',3),('b',2),('c',1),('d',4),('e',7)]
