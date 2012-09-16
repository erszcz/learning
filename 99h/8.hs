import Test.HUnit

compress (x:xs) = reverse . snd $ foldl f (x, [x]) xs
  where f (prev, acc) this | prev == this = (this, acc)
                           | otherwise    = (this, this : acc)

t expected actual = TestCase (assertEqual "" expected actual)

tests = TestList
  [ TestLabel "1" (t ["a"] (compress ["a","a","a"])),
    TestLabel "2" (t ["a","b"] (compress ["a", "a", "b"])),
    TestLabel "3" (t ["a","b","c","a","d","e"]
                     (compress ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"])) ]

main = do runTestTT tests
          return ()
