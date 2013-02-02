import System.Random as R

removeAt 1 (x:xs) = (x,xs)
removeAt i (x:xs) = let (rem,rest) = removeAt (i-1) xs in (rem, x:rest)

selectRandom :: [a] -> Int -> IO [a]
selectRandom xs n = doSelect xs n [] where
        doSelect xs 0 acc = do return acc
        doSelect xs n acc = do
            i <- getStdRandom $ randomR (1, length xs)
            let (y,ys) = removeAt i xs
            doSelect ys (n-1) (y:acc) >>= return 

randomPermute xs = do selectRandom xs (length xs) >>= return

main = do
    randomPermute "abc" >>= putStrLn . show
    randomPermute "ab" >>= putStrLn . show
    return ()
