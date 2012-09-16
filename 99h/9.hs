import Test.HUnit

pack (x:xs) = reverse $ (last : prev)
  where (last, prev) = foldl f ([x], []) xs
        f (p@(c:_), acc) e | c == e    = (e:p, acc)
                           | otherwise = ([e], p : acc)

t expected actual = TestCase (assertEqual "" expected actual)

tests = TestList
  [ TestLabel "1" (t ["aaa"] (pack ['a','a','a'])),
    TestLabel "2" (t ["aa","b"] (pack ['a', 'a', 'b'])),
    TestLabel "3" (t ["aaaa","b","cc","aa","d","eeee"]
                     (pack ['a','a','a','a','b','c','c','a','a','d',
                            'e','e','e','e'])),
    TestLabel "4" (t ["aaaa","b","cc","aa","d","eeee"]
                     (pack ['a','a','a','a','b','c','c','a','a',
                            'd','e','e','e','e'])) ]

main = do runTestTT tests
          return ()
