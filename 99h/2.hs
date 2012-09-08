import Control.Exception (assert)

myButLast [x,_]  = x
myButLast (_:xs) = myButLast xs

tests = [ assert (3   == myButLast [1,2,3,4]) True
        , assert ('b' == myButLast ['a','b','z']) True ]

main = do return $ all (==True) tests
