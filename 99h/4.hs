import Control.Exception (assert)

myLength [] = 0
myLength (_:xs) = 1 + myLength xs

tests = [ assert (4 == myLength [1,2,3,4]) True
        , assert (7 == myLength "haskell") True ]

main = do return $ all (==True) tests
