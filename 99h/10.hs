import Test.HUnit

encode [] = []
encode (x:xs) = reverse $ (last : prev)
  where (last, prev) = foldl f ((1,x), []) xs
        f ((n,c), acc) e | c == e    = ((n+1,c), acc)
                         | otherwise = ((1,e), (n,c) : acc)

-- well, it could be done easier:
-- encode xs = map (\x -> (length x,head x)) (group xs)

-- and more succinct:
-- [(length x, head x) | x <- group xs]

t expected actual = TestCase (assertEqual "" expected actual)

tests = TestList
  [ TestLabel "0" (t ([] :: [(Int,Char)]) (encode [])),
    TestLabel "1" (t [(3,'a')] (encode ['a','a','a'])),
    TestLabel "2" (t [(2,'a'),(1,'b')] (encode ['a','a','b'])),
    TestLabel "3" (t [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
                     (encode ['a','a','a','a','b','c','c','a','a','d',
                              'e','e','e','e'])),
    TestLabel "4" (t [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
                     (encode ['a','a','a','a','b','c','c','a','a',
                              'd','e','e','e','e'])) ]

main = do runTestTT tests
          return ()
