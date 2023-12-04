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
part1 = sum . map (score . winningValues) . parse
part2 = sum . play . parse

play :: [Card] -> [Int]
play cards = play' $ map (\c -> (length (winningValues c), 1)) cards
    where
        play' :: [(Int, Int)] -> [Int]
        play' []                             = []
        play' ((winningCount, cardCount):xs) = cardCount : play' rest
            where rest = mapN (\(wc, cc) -> (wc, cc + cardCount)) winningCount xs



winningValues :: Card -> [Int]
winningValues (Card _ winning my) = filter (`elem` winning) my

score :: [Int] -> Int
score []     = 0
score xs = 2 ^ (length xs - 1)



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

mapN :: (a -> a) -> Int -> [a] -> [a]
mapN f n xs = map f (take n xs) ++ (drop n xs)

