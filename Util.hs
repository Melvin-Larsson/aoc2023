module Util where

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn = splitOn' []
    where
        splitOn' :: Eq a => [a] -> [a] -> [a] -> [[a]]
        splitOn' aq _ [] = [aq]
        splitOn' aq c ss@(x:xs)
            | take (length c) ss == c = aq : splitOn' [] c (drop (length c) ss)
            | otherwise               = splitOn' (aq ++ [x]) c xs

splitIntoChunksOf :: Int -> [a] -> [[a]]
splitIntoChunksOf _ [] = []
splitIntoChunksOf n xs = take n xs : splitIntoChunksOf n (drop n xs)