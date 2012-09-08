import Control.Exception (assert)

myLast [x] = x
myLast (_:xs) = myLast xs

tests = [ assert (4   == myLast [1,2,3,4]) True
        , assert ('z' == myLast ['a','b','z']) True ]

main = do return $ all (==True) tests
