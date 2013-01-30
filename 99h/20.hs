import Test.HUnit

removeAt 1 (x:xs) = (x,xs)
removeAt i (x:xs) = let (rem,rest) = removeAt (i-1) xs in (rem, x:rest)

t expected actual = TestCase (assertEqual "" expected actual)

tests = TestList
  [ TestLabel "1" (t ('b', "acd") (removeAt 2 "abcd")),
    TestLabel "2" (t ('a', "") (removeAt 1 "a"))]

main = do runTestTT tests
          return ()
