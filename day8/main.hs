import Prelude hiding (Left, Right)

data Node = Node
    {
        name :: String,
        left :: String,
        right :: String
     } deriving Eq

data Command = Left | Right deriving (Show, Eq)

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStr "Part 1: "
    putStrLn $ show $ part1 input
    putStr "Part 2: "
    putStrLn $ show $ part2 input

part1 :: String -> Int
part1 s = length $ walkToEnd ((==) "ZZZ" . name) ns cs start
  where
    (cs, ns) = parse s
    start = head $ filter ((==) "AAA" . name) ns

part2 :: String -> Int
part2 = walkToEnd2 . parse



walkToEnd :: (Node -> Bool) -> [Node] -> [Command] -> Node -> [Node]
walkToEnd isEnd ns (c:cs) start
    | isEnd start   = []
    | otherwise = start : walkToEnd isEnd ns (cs ++ [c]) (next ns start c)

walkToEnd2 :: ([Command], [Node]) -> Int
walkToEnd2 (cs, ns) = foldr lcm 1 lengthsToEnds
    where
        starts = filter ((==) "A" . drop 2 . name) ns
        lengthsToEnds = map (length . walkToEnd ((==) "Z" . drop 2 . name) ns cs) starts


next :: [Node] -> Node -> Command -> Node
next (n1@(Node name1 left1 right1):xs) n2@(Node name2 left2 right2) command
  | command == Left && name1 == left2 = n1
  | command == Right && name1 == right2 = n1
  | otherwise = next xs n2 command



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
        left = take 3 leftRight
        right = drop 5 leftRight
