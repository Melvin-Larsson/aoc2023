import Prelude hiding (traverse)

import Data.Set (Set)
import Data.Foldable (find)
import Data.Maybe
import qualified Data.Set as Set

type Pos = (Int, Int)

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStr "Part 1: "
    putStrLn $ show $ part1 input
    putStr "Part 2: "
    putStrLn $ show $ part2 input

part1 :: String -> Int
part1 = (flip div) 2 . length . walkLoop . parse

part2 :: String -> Int
part2 input = width*height - length path - Set.size outsides
    where
        width = length (head world)
        height = length (world)

        world = parse input
        path = walkLoop $ parse input
        outsides = getOutsides world path

  

getOutsides :: [[Char]] -> [Pos] -> Set Pos
getOutsides world path
  | any (isOutside world) a = a
  | otherwise               = b
  where
        pathSet = Set.fromList path
        path2 = reverse path

        b = walkFrom world pathSet Set.empty (go path)
        a = walkFrom world pathSet Set.empty (go path2)

        go :: [Pos] -> Set Pos
        go []  = Set.empty
        go [_] = Set.empty
        go (p1:p2:ps)
          | not $ isInBounds world right = go (p2:ps)
          | not $ Set.member right pathSet = Set.insert right (go (p2:ps))
          | otherwise = go (p2:ps)
          where
            right = getRightPos (p1, p2)

isInBounds :: [[Char]] -> Pos -> Bool
isInBounds world (x,y) = x >= 0 && y >= 0 && x < (length (head world)) && y < (length world)

isOutside :: [[Char]] -> Pos -> Bool
isOutside world (x,y) = x <= 0 || y <= 0 || x >= (length (head world) - 1) || y >= (length world - 1)

getRightPos :: (Pos, Pos) -> Pos
getRightPos ((x1, y1), (x2, y2))
  | x1 == x2 && y2 > y1 = (x2 - 1, y1)
  | x1 == x2 && y2 < y1 = (x2 + 1, y1)
  | y1 == y2 && x2 > x1 = (x1, y2 + 1)
  | y1 == y2 && x2 < x1 = (x1, y2 - 1)
  | otherwise = error ("Unknown pos" ++ (show (x1, y1)) ++ "-" ++ (show (x2, y2)))

walkFrom :: [[Char]] -> Set Pos -> Set Pos -> Set Pos -> Set Pos
walkFrom cs path ps new
    | Set.null new = ps
    | otherwise = walkFrom cs path (Set.union ps new) allAdj
    where
        adj :: Pos -> Set Pos
        adj (x,y) = Set.fromList [(x', y') | y' <- [y-1..y+1], x' <- [x-1..x+1],
                                            x' >= 0 && y' >= 0 && y' < length cs && x' < length (head cs) &&
                                            (not $ Set.member (x',y') ps) && (not $ Set.member (x',y') path)]
        allAdj :: Set Pos
        allAdj = Set.unions $ Set.map adj new

walkLoop :: [[Char]] -> [Pos]
walkLoop world = start : (traverse world (start, p1))
    where
        start = findStart world
        (p1, p2) = getStartAdj world
        path = traverse world (start, p1)

traverse :: [[Char]] -> (Pos, Pos) -> [Pos]
traverse cs (p1, p2@(sx,sy))
    | cs @@ p2 == 'S' = []
    | otherwise = p2 : traverse cs (p2, p)
    where
        
        [p] = filter (\p -> (sx, sy) `elem` (map (add p) $ validPoses (cs @@ p)) 
                         && p `elem`(map (add p2) $ validPoses (cs @@ p2))
                     ) adj
        adj = [(x,y) | y <- [sy-1..sy+1], x <- [sx-1..sx+1], y >= 0, x >= 0, y < length cs, x < length (head cs), not ((x,y) `elem` [p1, p2])]


getStartAdj :: [[Char]] -> (Pos, Pos)
getStartAdj cs = (p1, p2)
    where
        [p1, p2] = filter (\p -> (sx, sy) `elem` (map (add p) $ validPoses (cs @@ p))) adj
        (sx, sy) = findStart cs
        adj = [(x,y) | y <- [sy-1..sy+1], x <- [sx-1..sx+1], y >= 0, x >= 0]

findStart :: [[Char]] -> Pos
findStart = findStart' (0,0)
    where
        findStart' (x,y) cs
          | x >= length (head cs) = findStart' (0, y+1) cs
          | cs @@ (x,y) == 'S' = (x,y)
          | otherwise = findStart' (x+1, y) cs


validPoses :: Char -> [Pos]
validPoses c = case c of
        '|' -> [(0,1), (0,-1)]
        '-' -> [(-1,0), (1,0)]
        'L' -> [(0,-1), (1,0)]
        'J' -> [(0,-1), (-1,0)]
        '7' -> [(-1, 0), (0, 1)]
        'F' -> [(1, 0), (0,1)]
        '.' -> []
        'S' -> [(x,y) | y <- [-1..1], x <- [-1..1], (x,y) /= (0,0), (x == 0 || y == 0)]

parse :: String -> [[Char]]
parse = lines

(@@) cs (x,y) =  (cs !! y) !! x
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
