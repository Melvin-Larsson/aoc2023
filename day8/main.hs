import Prelude hiding (Left, Right)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

data Node = Node
    {
        name :: String,
        left :: String,
        right :: String
     } 

instance Show Node
  where
    show (Node name _ _ ) = name

instance Eq Node
  where
    (==) n1 n2 = name n1 == name n2

instance Ord Node
  where
    (<=) n1 n2 = name n1 <= name n2

data Command = Left | Right deriving (Show, Eq)

main :: IO ()
main = do
--    input <- readFile "input.txt"
    input <- readFile "input.txt"
--    putStrLn $ show $ part1 input
    putStrLn $ show $ part2 input

part1 :: String -> Int
part1 = length . walkToEnd . parse


part2 :: String -> [Integer]
part2 s = walkToEnd3 $ (cs, ns)
  where
    (cs, ns) = parse s

walkToEnd3 :: ([Command], [Node]) -> [Integer]
walkToEnd3 (cs, ns) = [addToFast l1]
    where
        (l1, l2) = toLists lengthsBetweenEnd lengthsToEnd

        walkToEndOne ns (c:cs) n
          | drop 2 (name n) == "Z" = [n]
          | otherwise = n : walkToEndOne ns (cs ++ [c]) (next ns n c)

        

        toLists :: [(Node, Node, Int)] -> [(Node, Node, Int)] -> ([Integer], [Integer])
        toLists _ [] = ([],[])
        toLists xs (x@(start, end, length):ys) = ((fromIntegral length) : rest1, (fromIntegral length') : rest2)
          where
            (start', end', length') = getEnding xs x
            (rest1, rest2) = toLists xs ys

        getEnding :: [(Node, Node, Int)] -> (Node, Node, Int) -> (Node, Node, Int)
        getEnding [] x = error ("Could not find: " ++ (show x))
        getEnding ((start, end, length):xs) x@(_, end2, _ )
            | start == end2 = (start, end, length)
            | otherwise = getEnding xs x



        starts = findStarts ns
        ends = findEnds ns
        startEnds = map (\(Node name left right) -> (Node (name ++ "A") left right)) ends

        lengthsToEnd = map (\start -> let path = walkToEndOne ns cs start in (start, last path, length path - 1)) starts
        lengthsBetweenEnd = map (\start@(Node name left right) -> let path = walkToEndOne ns cs start in ((Node (init name) left right), last path, length path - 1)) startEnds

addToFast :: [Integer] -> Integer
addToFast [x]   = x
addToFast [x,y] = addToEqual [x,y] [x,y]
addToFast xs = addToEqual [resLeft, resRight] [resLeft, resRight]
  where
    l = length xs
    left = take (l `div` 2) xs
    right = drop (l `div` 2) xs
    resLeft = addToFast left
    resRight = addToFast right


addToEqual :: [Integer] -> [Integer] -> Integer
addToEqual xs ns
  | allEqual xs = head xs
  | otherwise = addToEqual (applyAt (\x -> x + (ns !! minIndex)) minIndex xs) ns
  where
    minIndex = indexOfMin xs



applyAt :: (Integer -> Integer) -> Int -> [Integer] -> [Integer]
applyAt f n xs= before ++ [f val] ++ after
  where
    before = take n xs
    after = drop (n+1) xs
    val = xs !! n

allEqual :: Eq a => [a] -> Bool
allEqual []  = True
allEqual [x] = True
allEqual (x:y:xs) = x == y && allEqual (y:xs)
        
indexOfMin :: [Integer] -> Int
indexOfMin xs = indexOfMin' 1 (head xs) 0 (tail xs)
  where
    indexOfMin' index min minIndex [] = minIndex
    indexOfMin' index min minIndex (x:xs)
      | x < min = indexOfMin' (index + 1) x index xs
      | otherwise = indexOfMin' (index + 1) min minIndex xs


walkToEnd2 :: ([Command], Map String Node) -> Int
walkToEnd2 (cs, ns) = walkToEnd' ns cs starts
    where
        walkToEnd' :: Map String Node -> [Command] -> [Node] -> Int
        walkToEnd' ns (c:cs) n
          | all ((==) "Z" . drop 2 . name) n = 0
          | otherwise = 1 + (walkToEnd' ns (cs ++ [c]) nextNodes)
          where
              nextNodes = map (\node -> next2 ns node c) n


        starts = findStarts $ map snd $ Map.toList ns

walkToEnd :: ([Command], [Node]) -> [Node]
walkToEnd (cs, ns) = walkToEnd' ns cs start
    where
        walkToEnd' ns (c:cs) n
          | name n == "ZZZ" = []
          | otherwise = n : walkToEnd' ns (cs ++ [c]) (next ns n c)

        start = findStart ns

findStart :: [Node] -> Node
findStart = head . filter ((==) "AAA" . name)

findStarts :: [Node] -> [Node]
findStarts = filter ((==) "A" . drop 2 . name)

findEnds :: [Node] -> [Node]
findEnds = filter ((==) "Z" . drop 2 . name)


next2 :: Map String Node -> Node -> Command -> Node
next2 map (Node _ left _) Left = fromJust $ Map.lookup left map
next2 map (Node _ _ right) Right = fromJust $ Map.lookup right map

toMap :: [Node] -> Map String Node
toMap []     = Map.empty
toMap (node@(Node name _ _ ):ns) = Map.insert name node (toMap ns)



next :: [Node] -> Node -> Command -> Node
next ((Node name1 left1 right1):xs) (Node name2 left2 right2) Left
  | name1 == left2 = Node name1 left1 right1
  | otherwise      =  next xs (Node name2 left2 right2) Left
next ((Node name1 left1 right1):xs) (Node name2 left2 right2) Right
  | name1 == right2 = Node name1 left1 right1
  | otherwise        =  next xs (Node name2 left2 right2) Right

parse :: String -> ([Command], [Node])
parse s = (map parseCommand commandStr, map parseNode nodesStrs)
    where
        commandStr = head $ lines s
        nodesStrs = drop 2 $ lines s


parseCommand :: Char -> Command
parseCommand 'L' = Left
parseCommand 'R' = Right

parseNode :: String -> Node
parseNode s = Node name left right
    where
        name = takeWhile (/= ' ') s
        leftRight = tail $ init $ dropWhile (/= '(') s
        [left, right] = splitOn ", " leftRight


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
