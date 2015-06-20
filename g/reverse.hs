rev n = rev' n [] where
    rev' [] acc = dropWhile (=='0') acc
    rev' (x:xs) acc = rev' xs (x:acc)

main = do
    putStrLn . show $ rev "123"
    putStrLn . show $ rev "23"
    putStrLn . show $ rev "230"
