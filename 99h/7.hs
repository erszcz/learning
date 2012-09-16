import Control.Exception (assert)

data List a = Elem a
            | List [List a]

myFlatten (Elem x) = [x]
myFlatten (List []) = []
myFlatten (List ((Elem x):xs)) = [x] ++ myFlatten (List xs)
myFlatten (List (x@(List _):xs)) = (myFlatten x) ++ myFlatten (List xs)

tests = [ assert ([5] == myFlatten (Elem 5)) True
        , assert ([1,2,3,4,5] == myFlatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])) True
        , assert (([] :: [Int]) == myFlatten (List [])) True ]

main = do return $ all (==True) tests
