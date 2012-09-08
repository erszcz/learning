import Control.Exception (assert)

elementAt (x:_)  1 = x
elementAt (_:xs) n = elementAt xs (n-1)

-- better from the solutions:
-- elementAt list n = list !! (n-1)

tests = [ assert (3   == elementAt [1,2,3,4] 3) True
        , assert ('e' == elementAt "haskell" 5) True ]

main = do return $ all (==True) tests
