type Pos = (Int, Int)
type Size = (Int, Int)

main :: IO ()
main = do
--    input <- readFile "input.txt"
    input <- readFile "input.txt"
    putStrLn $ show $ part1 input
    putStrLn $ show $ part2 input

part1 :: String -> Int
part1 s = (sum $ allDistances ps') `div` 2
    where
        cs = lines s
        width = length (head cs)
        height = length cs
        size = (width, height)
        ps = parse s
        xs = emptyX size ps
        ys = emptyY size ps
        newSize = (width + length xs, height + length ys)
        ps' = expandX (expandY ps ys) xs

part2 :: String -> Int
part2 s = 0

printWorld :: [Pos] -> Size -> String
printWorld ps size  = print' ps size (0,0)

print' ps (w,h) (x,y)
    | x >= w = '\n' : print' ps (w,h) (0, y+1)
    | y == h = []
    | (x,y) `elem` ps = '#' : print' ps (w,h) (x+1, y)
    | otherwise = '.' : print' ps (w,h) (x+1, y)

allDistances :: [Pos] -> [Int]
allDistances ps = concat $ map (distances ps) ps

n = 1000000

distances :: [Pos] -> Pos -> [Int]
distances [] _ = []
distances ((x1,y1):ps) (x2, y2) = (abs (x1-x2) + abs (y1 - y2)) : distances ps (x2,y2)

expandX :: [Pos] -> [Int] -> [Pos] 
expandX ps [] = ps
expandX ps (x:xs) = expandX (map (\(x',y') -> if x < x' then (x'+n-1, y') else (x',y')) ps) (map (+(n-1)) xs)

expandY :: [Pos] -> [Int] -> [Pos] 
expandY ps [] = ps
expandY ps (y:ys) = expandY (map (\(x',y') -> if y < y' then (x', y' + n-1) else (x',y')) ps) (map (+(n-1)) ys)

emptyX :: Size -> [Pos] -> [Int]
emptyX (w, h) ps = [x | x <- [0..w-1], not $ any (\(x',_) -> x' == x) ps]

emptyY :: Size -> [Pos] -> [Int]
emptyY (w, h) ps = [y | y <- [0..h-1], not $ any (\(_,y') -> y' == y) ps]

parse :: String -> [Pos]
parse = parse' (0,0) . lines

parse' :: Pos -> [[Char]] -> [Pos]
parse' _ []           = []
parse' (x,y) ([]:row) = parse' (0, y+1) row
parse' (x,y) ((c:cs):row)
  | c == '#' = (x,y) : parse' (x+1, y) (cs:row)
  | otherwise = parse' (x+1, y) (cs:row)


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
