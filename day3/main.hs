import Data.Char
import Data.List (nub)

type Pos = (Int, Int)

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStr "Part 1: "
    putStrLn $ show $ part1 input
    putStr "Part 2: "
    putStrLn $ show $ part2 input

part1, part2 :: String -> Int
part1 = sum . getParts . lines
part2 = sum . getGearRatio . lines

getParts :: [[Char]] -> [Int]
getParts cs = map (head . getPart cs) $ map head $ filter (isPart cs) $ getPoses cs

getGearRatio :: [[Char]] -> [Int]
getGearRatio cs = map (product . getAdjacentPart cs) $ filter isGear poses
    where
        height = length cs
        width = length (head cs)
        poses = [(x,y) | x <- [0..width-1], y <- [0..height-1]]
        isGear p = cs @@ p == '*' && length (getAdjacentPart cs p) == 2


getAdjacentPart :: [[Char]] -> Pos -> [Int]
getAdjacentPart cs p = nub $ concat $ map (getPart cs) $ adjacentPos cs p

getPoses :: [[Char]] -> [[Pos]]
getPoses = concat . map (\(y, r) -> getRow (0,y) r) . zip [0..]
    where
        getRow :: Pos -> [Char] -> [[Pos]]
        getRow _ [] = []
        getRow (x,y) s@(c:cs)
            | null chars = getRow (x+1,y) cs
            | otherwise = [(x2, y) | x2 <- [x..x+l]] : getRow (x+l+1, y) rest
            where
                chars = takeWhile (isDigit) s
                rest = dropWhile (isDigit) s
                l = length chars - 1

isPart :: [[Char]] -> [Pos] -> Bool
isPart xs ps = any notEmpty adjPoses 
  where
    adjPoses = filter (\p -> not (p `elem` ps)) $ concat $ map (\p -> adjacentPos xs p) ps
    notEmpty p = xs @@ p /= '.'

adjacentPos :: [[a]] -> Pos -> [Pos]
adjacentPos cs (x,y) = [(x2, y2) | y2 <- [y-1..y+1], x2 <- [x-1..x+1], isInBounds cs (x2, y2), (x2, y2) /= (x,y)]

isInBounds :: [[a]] -> Pos  -> Bool
isInBounds cs (x,y) = y >= 0 && y < length cs && x >= 0 && x < length (head cs)

getPart :: [[Char]] -> Pos -> [Int]
getPart cs (x,y)
  | isInBounds cs (x,y) && isDigit (cs @@ (x,y)) = [read (leftDigs ++ rightDigs)]
  | otherwise                                       = []
    where
        row = cs !! y
        left = take x row
        right = drop x row
        leftDigs = reverse $ takeWhile isDigit $ reverse left
        rightDigs = takeWhile isDigit right


(@@) :: [[a]] -> Pos -> a
(@@) xs (x,y) = (xs !! y) !! x
