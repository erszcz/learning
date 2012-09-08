import Control.Exception (assert)

myReverse xs = myReverse' xs []

myReverse' []     acc = acc
myReverse' (x:xs) acc = myReverse' xs (x:acc)

tests = [ assert ([1,2,3] == myReverse [3,2,1]) True
        , assert ("rak"   == myReverse "kar")   True ]

main = do return $ all (==True) tests
