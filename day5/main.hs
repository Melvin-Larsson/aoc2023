import Data.Char

data Conv = Conv Int Int Int deriving Show
data Range = Range Int Int deriving Show
type Seed = Range

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStr "Part 1: "
    putStrLn $ show $ part1 input
    putStr "Part 2: "
    putStrLn $ show $ part2 input

part1 :: String -> Int
part1 s = minimum $ map (\(Range s _) -> s) $ convertAll ss convs
 where
     (seeds, convs) = parse s
     ss = foldr (\(Range start end) r -> Range start 1 : Range end 1 : r) [] seeds

part2 :: String -> Int
part2 s = minimum $ map (\(Range s _) -> s) $ convertAll seeds convs
    where
        (seeds, convs) = parse s

convertAll :: [Range] -> [[Conv]] -> [Range]
convertAll vs convs = concat $ map (\v -> convertFull v convs) vs

convertFull :: Range -> [[Conv]] -> [Range]
convertFull v []     = [v]
convertFull v (c:cs) = concat $ map (\r -> convertFull r cs) $ convertOnce v c

convertOnce :: Range -> [Conv] -> [Range]
convertOnce v cs = conv cs v cs

conv :: [Conv] -> Range -> [Conv] -> [Range]
conv _ (Range _ 0) _ = []
conv _ v [] = [v]
conv c r@(Range rStart rRange) (con@(Conv value start range):cs)
  | overlaps (s1, e1) (s2, e2) = new : (concat $ map (\r -> conv c r c) split)
  | otherwise = conv c r cs
    where
        (split, new) = splitRange r con
        (s1, e1) = (rStart, rStart + rRange - 1)
        (s2, e2) = (start, start + range - 1)

splitRange :: Range -> Conv -> ([Range], Range)
splitRange (Range rStart rRange) (Conv value start range)
  | s1 < s2 && e1 > e2 = ([Range beforeStart beforeRange, Range afterStart afterRange], Range middleVal middleRange)
  | s1 >= s2 && e1 <= e2 = ([], Range middleVal middleRange)
  | s1 < s2 = ([Range beforeStart beforeRange], Range middleVal middleRange)
  | e1 > e2 = ([Range afterStart afterRange], Range middleVal middleRange)
    where
        s1 = rStart;
        e1 = rStart + rRange - 1;

        s2 = start;
        e2 = start + range - 1

        beforeStart = rStart
        beforeRange = start - rStart

        afterStart = start + range
        afterRange = rStart + rRange - start - range

        middleStart = max start rStart
        middleEnd   = min (rStart + rRange) (start + range)
        middleVal = middleStart - start + value
        middleRange = middleEnd - middleStart


overlaps :: (Int, Int) -> (Int, Int) -> Bool
overlaps (a,b) (x,y) = y >= a && x <= b


parse :: String -> ([Seed], [[Conv]])
parse s = (seeds, map parseConvs chunks)
    where
        rows = lines s
        seeds = parseSeeds (rows !! 0)
        chunks = map (filter (\s -> isDigit (s !! 0))) $ splitOn [""] rows

parseSeeds :: String -> [Seed]
parseSeeds s = p' numbers
    where
        numbersString = drop 2 $ dropWhile (/=':') s
        numbers = splitOn " " numbersString

        p' :: [String] -> [Seed]
        p' []       = []
        p' (x:y:xs) = Range (read x) (read y) : p' xs


parseConvs :: [String] -> [Conv]
parseConvs = map parseConv

parseConv :: String -> Conv
parseConv row = Conv (read start) (read end) (read value)
    where
        [start, end, value] = splitOn " " row


splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn = splitOn' []
    where
        splitOn' :: Eq a => [a] -> [a] -> [a] -> [[a]]
        splitOn' aq _ [] = [aq]
        splitOn' aq c ss@(x:xs)
            | take (length c) ss == c = aq : splitOn' [] c (drop (length c) ss)
            | otherwise               = splitOn' (aq ++ [x]) c xs
