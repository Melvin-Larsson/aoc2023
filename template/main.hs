main :: IO ()
main = do
--    input <- readFile "input.txt"
    input <- readFile "test.txt"
    putStrLn $ show $ part1 input
    putStrLn $ show $ part2 input

part1 :: String -> Int
part1 s = 0

part2 :: String -> Int
part2 s = 0


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
