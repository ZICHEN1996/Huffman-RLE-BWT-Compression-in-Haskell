-------------------------------------------------------------------------
--
--  The main module of the Huffman example
--  U6161816
-------------------------------------------------------------------------
module Huffman
  ( encode
  , decode
  ) where

import CodeTable (codeTable)
import Coding (encodeMessage, decodeMessage)
import Frequency (frequency)
import MakeTree (makeTree)
import Types (Tree(Leaf, Node), Bit(L, R), HCode, Table)

--encode a list of characters, producing a Huffman tree for decoding and the Huffman Code
encode
  :: Ord a
  => [a] -> (Tree a, HCode)
encode xs = (tree, bits)
  where
    freqs = frequency xs
    tree = makeTree freqs
    table = codeTable tree
    bits = encodeMessage table xs

--decode using a Huffman tree and Huffman code
decode
  :: Ord a
  => (Tree a, HCode) -> [a]
decode (tree, bits) = decodeMessage tree bits

-- Examples
-- ^^^^^^^^

-- Putting together frequency calculation and tree conversion   
codes
  :: Ord a
  => [a] -> Tree a
codes = makeTree . frequency

-- The coding table generated from the text "there is a green hill".            
tableEx :: Table Char
tableEx = codeTable (codes "there is a green hill")

-- The Huffman tree generated from the text "there is a green hill",
-- from which tableEx is produced by applying codeTable.
treeEx :: Tree Char
treeEx = codes "there is a green hill"

-- A message to be coded.
message :: String
message = "there are green hills here"

-- The message in code.
coded :: HCode
coded = encodeMessage tableEx message

-- The coded message decoded.
decoded :: String
decoded = decodeMessage treeEx coded

test :: Bool
test = decode (encode "fkasngaj;gkbamnioajknbal;jhkmdnasjhl") == "fkasngaj;gkbamnioajknbal;jhkmdnasjhl"

main = print decoded
