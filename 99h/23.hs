import System.Random as R

selectRandom :: [a] -> Int -> IO [a]
selectRandom xs n = do
    g <- newStdGen
    return [ xs !! r | r <- take n $ randomRs (0,length xs - 1) g ]

main = do
    selectRandom "abcdefgh" 3 >>= putStrLn
    selectRandom "abcdefgh" 6 >>= putStrLn
    selectRandom "abcdefgh" 2 >>= putStrLn
    return ()
 
-- RNG usage example
   
-- rollDice :: IO Int
-- rollDice = getStdRandom (randomR (1,6))

-- main = do rollDice >>= putStrLn . show
--           return ()
