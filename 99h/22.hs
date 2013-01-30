import Test.HUnit

-- range i k = [i..k]

range i k | i == k = [k]
          | i /= k = let next = if i < k then succ else pred in
                     i : (range (next i) k)

t expected actual = TestCase (assertEqual "" expected actual)

tests = TestList
  [ TestLabel "1" (t [4,5,6,7,8,9] (range 4 9)),
    TestLabel "1" (t [9,8,7,6,5,4] (range 9 4)) ]

main = do runTestTT tests
          return ()
