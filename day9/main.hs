type Sequence = [Int]

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStr "Part 1: "
    putStrLn $ show $ part1 input
    putStr "Part 2: "
    putStrLn $ show $ part2 input

part1, part2 :: String -> Int
part1 = sum . map newValue . parse
part2 = sum . map (newValue . reverse) . parse

newValue :: Sequence -> Int
newValue = sum . map last . extend
    where
        extend s
            | all (==0) s = [s]
            | otherwise   = s : extend (zipWith (-) (tail s) s)

parse :: String -> [Sequence]
parse = map (map read . splitOn " ") . lines

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn = splitOn' []
    where
        splitOn' :: Eq a => [a] -> [a] -> [a] -> [[a]]
        splitOn' aq _ [] = [aq]
        splitOn' aq c ss@(x:xs)
            | take (length c) ss == c = aq : splitOn' [] c (drop (length c) ss)
            | otherwise               = splitOn' (aq ++ [x]) c xs
