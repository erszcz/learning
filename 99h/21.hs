import Test.HUnit

insertAt e [] _ = [e]
insertAt e xs 1 = e:xs
insertAt e (x:xs) i = x : (insertAt e xs (i-1))

-- solution
-- insertAt :: a -> [a] -> Int -> [a]
-- insertAt x xs n = let (ys,zs) = splitAt (n-1) xs in ys++x:zs

t expected actual = TestCase (assertEqual "" expected actual)

tests = TestList
  [ TestLabel "1" (t ("aXbcd") (insertAt 'X' "abcd" 2))
  , TestLabel "2" (t ("X") (insertAt 'X' "" 1))
  , TestLabel "3" (t ("X") (insertAt 'X' "" 2)) ]

main = do runTestTT tests
          return ()
