-------------------------------------------------------------------------
--  
--         Coding.hs                            
--         U6161816
--         Huffman coding in Haskell.                   
--
--         The top-level functions for coding and decoding.
--
-------------------------------------------------------------------------
module Coding
  ( encodeMessage
  , decodeMessage
  ) where

import Types (Tree(Leaf, Node), Bit(L, R), HCode, Table)
import Prelude hiding (lookup)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
{-# ANN module ("HLint: ignore Eta reduce"::String) #-}

-- Code a message according to a table of codes.            
encodeMessage
  :: Ord a
  => Table a -> [a] -> HCode
encodeMessage _ [] = []
encodeMessage table (x:xs) = (lookup table x) ++ (encodeMessage table xs)


-- lookup looks up the meaning of an individual char in a Table.
lookup
  :: Ord a
  => Table a -> a -> HCode
lookup m c = fromMaybe (error "lookup") (Map.lookup c m)

-- Decode a message according to a tree.                
--                              
-- The first tree argument is constant, being the tree of codes;
-- the second represents the current position in the tree relative  
-- to the (partial) HCode read so far.               
decodeMessage
  :: Ord a
  => Tree a -> HCode -> [a]
decodeMessage tree code = helper tree code []
    where
        helper (Leaf _ x) hs string = helper tree hs (x:string)
        helper _ [] string = reverse string --We cannot put this line on the top! Otherwise the last character may be missing due to the operation order.
        helper (Node _ l r) (h:hs) string =
            case h of
                L -> helper l hs string
                R -> helper r hs string

--test our decodeMessage
test1 :: Bool
test1 = decodeMessage (Node 10 (Leaf 6 'a') (Node 4 (Leaf 3 'b') (Leaf 1 'c'))) [L,L,L,L,R,R,R,L,R,L,L,L,R,L] == "aaaacbbaab"

--test out encodeMessage
test2 :: Bool
test2 = encodeMessage (Map.fromList [('a',[L]),('b',[R,L]),('c',[R,R])]) "aaaacbbaab" == [L,L,L,L,R,R,R,L,R,L,L,L,R,L]