-------------------------------------------------------------------------
--
--         RLE.hs
--         U6161816
--         Module for Run Length Encoding
--
-------------------------------------------------------------------------

module RLE where

import Data.List

--encode a list of characters with RLE
rle_encode :: Eq a => [a] -> [(a,Int)]
rle_encode list
    | length list <= 255 = helper 0 list
    | length list > 255 = helper 0 (take 255 list) ++ rle_encode (drop 255 list)
    where
        helper n [x] = [(x,n+1)]
        helper n (x:xs)
            | x == head xs = helper (n+1) xs
            | otherwise = (x,1+n) : (helper 0 xs)

--decode a RLE encoded message
rle_decode :: [(a,Int)] -> [a]
rle_decode [] = []
rle_decode ((char,0):xs) = rle_decode xs
rle_decode ((char,num):xs) = char : rle_decode ((char,num-1):xs)

--test
test :: Bool
test = rle_decode (rle_encode "sahganxzjkhukshagklejnkuowpaeehnwjhoiajjkbajkhq; jiqhihgqepwijhoquh9qujhiowhioqbuighuivjahfghakwuqihykoqjypoqjyq[pjyknmklvbuiqocpjojctxjiqjiwitejqiowtjeiqjhajgiuagfoahqqetfguqwgqwruih173561986yijkoeawjioahkalnvgjkhaujbhjagfyuftq7ogeuwiqgheuigfuig1yuuuiguigwueg1uyr8912y58912y89y89261uy89u68921y6891yu689yihjgkabgjkfn,fmjl;ykp[4o50-i3087902u970jop12jk1hioyup1j1;hy1iuj5iouyiohjyp24u7yoijhioyh") == "sahganxzjkhukshagklejnkuowpaeehnwjhoiajjkbajkhq; jiqhihgqepwijhoquh9qujhiowhioqbuighuivjahfghakwuqihykoqjypoqjyq[pjyknmklvbuiqocpjojctxjiqjiwitejqiowtjeiqjhajgiuagfoahqqetfguqwgqwruih173561986yijkoeawjioahkalnvgjkhaujbhjagfyuftq7ogeuwiqgheuigfuig1yuuuiguigwueg1uyr8912y58912y89y89261uy89u68921y6891yu689yihjgkabgjkfn,fmjl;ykp[4o50-i3087902u970jop12jk1hioyup1j1;hy1iuj5iouyiohjyp24u7yoijhioyh"