import System.Random as R

selectRandom :: Int -> Int -> IO [Int]
selectRandom n max = do
    g <- newStdGen
    return $ take n $ randomRs (1, max) g

main = do
    selectRandom 5 100 >>= putStrLn . show
    selectRandom 6 49 >>= putStrLn . show
    return ()
