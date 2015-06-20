import Test.HUnit

isPrime 2 = True
isPrime 3 = True
isPrime n | n `rem` 2 == 0 = False
          | n `rem` 3 == 0 = False
          | otherwise = all (== False) candidates where
    candidates = [ n `rem` i == 0
                 | i <- [5..(round $ sqrt (fromIntegral n))],
                   i `rem` 2 /= 0, i `rem` 3 /= 0 ]

t expected actual = TestCase (assertEqual "" expected actual)

firstPrimes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53,
               59, 61, 67, 71]

tests = TestList
  [ TestLabel "1" (t True (all (== True) $
                            map isPrime firstPrimes))
  , TestLabel "2" (t True (all (== False) $
                            map isPrime [4,6..20])) ]

main = do runTestTT tests
          return ()


