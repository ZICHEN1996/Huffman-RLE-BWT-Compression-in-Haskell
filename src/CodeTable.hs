-------------------------------------------------------------------------
--
--         CodeTable.hs
--         U6161816
--         Converting a Huffman tree to a table.
--
-------------------------------------------------------------------------
module CodeTable
  ( codeTable
  ) where

import Types (Tree(Leaf, Node), Bit(L, R), HCode, Table)
import Data.Map (fromList, assocs)

-- Making a table from a Huffman tree.
codeTable :: Ord a => Tree a -> Table a
codeTable t = fromList (convert [] t)

-- Auxiliary function used in conversion to a table. The first argument is
-- the HCode which codes the path in the tree to the current Node, and so
-- codeTable is initialised with an empty such sequence.
convert :: Ord a => HCode -> Tree a -> [(a,HCode)]
convert h t =
    case t of
        Node _ left right -> (convert (h ++ [L]) left) ++ (convert (h ++ [R]) right)
        Leaf _ x          -> [(x,h)]

--test whether codeTable functions given a sample tree
test :: Bool
test = codeTable (Node 10 (Leaf 6 'a') (Node 4 (Leaf 3 'b') (Leaf 1 'c'))) == fromList [('a',[L]),('b',[R,L]),('c',[R,R])]
