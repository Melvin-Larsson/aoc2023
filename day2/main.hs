data Game = Game
    {
        nr    :: Int,
        red   :: [Int],
        green :: [Int],
        blue  :: [Int]
    }
    deriving Show

data Color = Red | Green | Blue

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStr "Part 1: "
    putStrLn $ show $ part1 input
    putStr "Part 2: "
    putStrLn $ show $ part2 input

part1, part2 :: String -> Int
part1 = sum . map nr . filter (\(Game nr r g b) -> (all (<=12) r) && all (<=13) g && all (<=14) b) . parse
part2 = sum . map power . map minGame . parse

power :: Game -> Int
power (Game _ [r] [g] [b]) = r * g * b

minGame :: Game -> Game
minGame (Game nr r g b) = (Game nr [maximum r] [maximum g] [maximum b])

parse :: String -> [Game]
parse = map parseRow . lines

parseRow :: String -> Game
parseRow row = getGame (read idStr) $ map parseColor parts
    where
        idStr = drop 5 $ takeWhile (/=':') row
        rest  = drop 2 $ dropWhile (/=':') row
        parts = concat $ map (splitOn ", ") $ splitOn "; " rest

getGame :: Int -> [(Color, Int)] -> Game
getGame nr []         = Game nr [] [] []
getGame nr (c:xs)     =  appendColor (getGame nr xs) c

appendColor :: Game -> (Color, Int) -> Game
appendColor (Game nr r g b) (c, n) = case c of
            Red   -> (Game nr (n:r) g b) 
            Green -> (Game nr r (n:g) b)
            Blue  -> (Game nr r g (n:b))

parseColor :: String -> (Color, Int)
parseColor s = case color of
    " blue"  -> (Blue, nr)
    " red"   -> (Red, nr)
    " green" -> (Green, nr)
    where
        (nrStr, color) = break (==' ') s
        nr = read nrStr


splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn = splitOn' []
    where
        splitOn' :: Eq a => [a] -> [a] -> [a] -> [[a]]
        splitOn' aq _ [] = [aq]
        splitOn' aq c ss@(x:xs)
            | take (length c) ss == c = aq : splitOn' [] c (drop (length c) ss)
            | otherwise               = splitOn' (aq ++ [x]) c xs
