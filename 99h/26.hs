import Test.HUnit

combinations n xs = []

t expected actual = TestCase (assertEqual "" expected actual)

tests = TestList
  [ TestLabel "1" (t ('b', "acd") (removeAt 2 "abcd")),
    TestLabel "2" (t ('a', "") (removeAt 1 "a"))]

main = do runTestTT tests
          return ()
