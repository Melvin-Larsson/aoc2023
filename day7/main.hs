import Data.List
import Data.Char


data Hand = Hand Type [Label] Int deriving (Show, Eq, Ord)

data Type = High | One | Two | Three | FullHouse | Four | Five deriving (Show, Eq ,Ord)

data Label = Numeric Int | T | J | Q | K | A deriving (Show, Eq, Ord)

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStr "Part 1: "
    putStrLn $ show $ part1 input
    putStr "Part 2: "
    putStrLn $ show $ part2 input

part1, part2 :: String -> Int
part1 = score compare . parse getLabelCount1
part2 = score comp . parse getLabelCount2

score :: (Hand -> Hand -> Ordering) -> [Hand] -> Int
score c = sum . zipWith (\n (Hand  _ _ v) -> n * v) [1..] . sortBy c


getLabelCount1 :: String -> [Int]
getLabelCount1 = reverse . sort . map snd . map (\xs -> (head xs, length xs)) . group . sort

getLabelCount2 :: String -> [Int]
getLabelCount2 s
    | null count = [jCount]
    | otherwise  = (c + jCount) : cs
    where
        count = getLabelCount1 . filter (/= 'J') $ s
        jCount = length . filter (=='J') $ s
        (c:cs) = count

comp :: Hand -> Hand -> Ordering
comp (Hand t1 s1 _) (Hand t2 s2 _)
  | t1 /= t2   = compare t1 t2
  | diff1 == J = LT
  | diff2 == J = GT
  | otherwise  = compare diff1 diff2
    where
        (diff1, diff2) = (head $ dropWhile (\(n1, n2) -> n1 == n2) $ zip s1 s2) 


parse :: (String -> [Int]) -> String -> [Hand]
parse f = map (parseRow f) . lines

parseRow :: (String -> [Int]) -> String -> Hand
parseRow f s = Hand (getType f t) (map getLabel t) (read num)
    where
        t = takeWhile (/= ' ') s
        num = tail $ dropWhile (/= ' ') s

getLabel :: Char -> Label
getLabel c = case c of
    'A' -> A
    'K' -> K
    'Q' -> Q
    'J' -> J
    'T' -> T
    _  -> Numeric (ord c - ord '0')

getType :: (String -> [Int]) -> String -> Type
getType f s = case f s of
    [5]          -> Five
    [4, 1]       -> Four
    [3, 2]       -> FullHouse
    [3, 1, 1]    -> Three
    [2, 2, 1]    -> Two
    [2, 1, 1, 1] -> One
    [1,1,1,1,1]  -> High
