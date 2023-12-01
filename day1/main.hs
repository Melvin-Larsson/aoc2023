import Data.List
import Data.Char

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStr "Part 1: "
    putStrLn $ show $ part1 input
    putStr "Part 2: "
    putStrLn $ show $ part2 input

part1,part2 :: String -> Int
part1 = sum . map points . parse startingDigit1
part2 = sum . map points . parse startingDigit2


points :: (Int, Int) -> Int
points (i1, i2) = i1 * 10 + i2


parse :: (String -> Maybe Int) -> String -> [(Int, Int)]
parse f = map (\xs -> (head xs, last xs)) . map (digits f) . lines

digits :: (String -> Maybe Int) -> String -> [Int]
digits _ [] = []
digits f s = case f s of
      Nothing -> digits f (tail s)
      Just n -> n : digits f (tail s)

startingDigit1 :: String -> Maybe Int
startingDigit1 (c:cs)
  | isDigit c = Just (ord c - ord '0')
  | otherwise = Nothing

startingDigit2 :: String -> Maybe Int
startingDigit2 s@(c:cs)
  | isDigit c = Just (ord c - ord '0')
  | otherwise = indexOfPrefix numbers s >>= Just . (+1)

indexOfPrefix :: Eq a => [[a]] -> [a] -> Maybe Int
indexOfPrefix [] _ = Nothing
indexOfPrefix (prefix:ps) val
  | isPrefixOf prefix val = Just 0
  | otherwise = indexOfPrefix ps val >>= Just . (+1)

numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
