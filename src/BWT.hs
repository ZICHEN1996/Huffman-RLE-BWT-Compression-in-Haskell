-------------------------------------------------------------------------
--
--  The module of BWT encoding
--  U6161816
-------------------------------------------------------------------------

module BWT where

import Data.Binary
import Data.List

import RLE
{-# ANN module ("HLint: ignore Use foldr"::String) #-}

data BWTValue a
  = EOL -- End of line
  | Value a -- Wraps a character from the original string.
  deriving (Ord, Eq)

-- Transform a string into a string with the `special end character'
mark_end :: [a] -> [BWTValue a]
mark_end [] = [EOL]
mark_end (x:xs) = (Value x) : (mark_end xs)

-- Produce all rotations of the input string.
-- Hint: look at the functions `inits` and `tails` and zipWith
make_rotations :: [BWTValue a] -> [[BWTValue a]]
make_rotations (EOL:xs) = [EOL:xs]
make_rotations (x:xs) = (x:xs) : make_rotations (xs ++ [x])

-- Sort the list of rotated 
sort_rotations
  :: Ord a
  => [[BWTValue a]] -> [[BWTValue a]]
sort_rotations = sortBy helper
    where
        helper [] [] = EQ
        helper (x:xs) (y:ys)
            | x < y = LT
            | x > y = GT
            | x == y = helper xs ys

-- Retrive the last BWTValue a from each of the sorted rotations
get_lasts :: [[BWTValue a]] -> [BWTValue a]
get_lasts [] = []
get_lasts (x:xs) = (last x) : (get_lasts xs)

-- Join it all together
bwt_encode
  :: Ord a
  => [a] -> [BWTValue a]
bwt_encode = get_lasts.sort_rotations.make_rotations.mark_end

-- Undo the BWT process. The Int is just the length of the input
make_bwt_table
  :: Ord a
  => [BWTValue a] -> [[BWTValue a]] -- type signature altered
make_bwt_table bwt = helper [] (length bwt)
    where
        helper table 0 = table
        helper table iteration = helper (sort_rotations (addbwt bwt table)) (iteration - 1)
        addbwt [] [] = []
        addbwt (x:xs) table0 = case table0 of
            [] -> splitlist bwt
                where
                splitlist [] = []
                splitlist (x:xs) = [x] : splitlist xs
            (y:ys) -> (x:y) : addbwt xs ys

-- Return the decoded message to its original type
remove_BWTValues :: [BWTValue a] -> [a]
remove_BWTValues [] = []
remove_BWTValues (x:xs) = case x of
    Value n -> n : remove_BWTValues xs
    EOL -> remove_BWTValues xs

-- Find the rotation that ends with EOL
find_EOL_line :: Eq a => [[BWTValue a]] -> [BWTValue a]
find_EOL_line (x:xs)
    | (last x) == EOL = x
    | otherwise = find_EOL_line xs

-- Decode BWT
bwt_decode
  :: Ord a
  => [BWTValue a] -> [a]
bwt_decode = remove_BWTValues.find_EOL_line.make_bwt_table

testbwt :: Bool
testbwt = bwt_decode (bwt_encode "ahasjkghdakugnkljpowkorpsjoijhpjrklnjkHPRHNQ") == "ahasjkghdakugnkljpowkorpsjoijhpjrklnjkHPRHNQ"

instance Show a =>
         Show (BWTValue a) where
  show EOL = "@"
  show (Value x) = show x

instance Binary a =>
         Binary (BWTValue a) where
  put EOL = putWord8 0
  put (Value x) = putWord8 1 >> put x
  get = do
    n <- getWord8
    case n of
      0 -> return EOL
      1 -> fmap Value get
      _ -> error $ "get(BWTValue a): unexpected tag: " ++ show n

instance Functor BWTValue where
  fmap _ EOL = EOL
  fmap f (Value x) = Value (f x)
