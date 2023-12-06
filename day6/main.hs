input = [(41, 249), (77,1362), (70, 1127), (96, 1011)]
input2 = [(41777096, 249136211271011)]

main :: IO ()
main = do
    putStr "Part 1: "
    putStrLn $ show $ rangeSize input
    putStr "Part 2: "
    putStrLn $ show $ rangeSize input2

rangeSize :: [(Integer, Integer)] -> Integer
rangeSize = product . map (\(s,e) -> e - s + 1) . map calcRange

calcRange :: (Integer, Integer) -> (Integer, Integer)
calcRange (time, distance) = (ceiling minTime, floor maxTime)
    where
        timeF = fromIntegral time
        distanceF = fromIntegral distance

        (maxTime, minTime) = timeF/2 +- sqrt(timeF^^2 / 4 - distanceF)

infixl 6 +-
(+-) a b = (a + b, a - b)
