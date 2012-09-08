import Control.Exception (assert)

isPalindrome (x:xs)   = isPalindrome' xs [x]

isPalindrome' [x] [y] | x == y       = True
isPalindrome' [x] ys                 = False
isPalindrome' (x:xs) (y:ys) | x == y = isPalindrome' xs ys
isPalindrome' (x:xs) ys              = isPalindrome' xs ys || isPalindrome' xs (x:ys)

tests = [ assert (False == isPalindrome "hamak")       True
        , assert (True  == isPalindrome "anna")        True
        , assert (True  == isPalindrome "madamimadam") True
        , assert (False == isPalindrome [1,2,3])       True ]

main = do return $ all (==True) tests
