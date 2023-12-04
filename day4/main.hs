import Data.List

data Card = Card Int [Int] [Int] deriving Show

instance Eq Card where
    (==) (Card c1 _ _ ) (Card c2 _ _) = c1 == c2

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStr "Part 1: "
    putStrLn $ show $ part1 input
    putStr "Part 2: "
    putStrLn $ show $ part2 input

part1, part2 :: String -> Int
part1 = sum . map (score . myWinningValues) . parse
part2 = sum . map snd . play . parse

play :: [Card] -> [(Card, Int)]
play cs = playAll cardCount cardCount
    where
        cardCount = map (\c -> (c,1)) cs

playAll :: [(Card, Int)] -> [(Card, Int)] -> [(Card, Int)]
playAll all []     = all
playAll all (c:cs) = playAll played newCs
    where
        played = play' all c
        diff = length all - length cs
        newCs = drop diff played

play' :: [(Card, Int)] -> (Card, Int) -> [(Card, Int)]
play' cards (card, n) = map (\(c,i) -> if ((c,i) `elem` winning) then (c,i+n) else (c, i)) cards
    where
        index = snd $ head $ dropWhile (\((c,_), _) -> c /= card) $ zip cards [0..]
        winns = length $ myWinningValues card
        winning = take winns $ drop (index + 1) cards

score :: [Int] -> Int
score []     = 0
score [_]      = 1
score (x:xs) = 2 * score xs

myWinningValues :: Card -> [Int]
myWinningValues (Card _ winning my) = filter (\v -> v `elem` winning) my

parse :: String -> [Card]
parse = map parseCard . lines

parseCard :: String -> Card
parseCard s = Card nr winning my
    where
        [card, rest] = splitOn ": " s
        [winningStr, myStr] = splitOn " | " rest
        winning = map read $ filter (/= "") $ splitOn " " winningStr
        my = map read $ filter (/= "") $ splitOn " " myStr
        nr = read $ drop 5 card


splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn = splitOn' []
    where
        splitOn' :: Eq a => [a] -> [a] -> [a] -> [[a]]
        splitOn' aq _ [] = [aq]
        splitOn' aq c ss@(x:xs)
            | take (length c) ss == c = aq : splitOn' [] c (drop (length c) ss)
            | otherwise               = splitOn' (aq ++ [x]) c xs
