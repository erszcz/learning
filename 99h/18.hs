import Test.HUnit

-- slice l from to = let inBetween = (\e -> e >= from && e <= to) in
--     map fst $ filter (inBetween . snd) $ zip l [1..]

-- solution
slice l i k | i>0 = take (k-i+1) $ drop (i-1) l

t expected actual = TestCase (assertEqual "" expected actual)

tests = TestList
  [ TestLabel "1"
        (t ("cdefg")
        (slice ['a','b','c','d','e','f','g','h','i','k'] 3 7)) ]

main = do runTestTT tests
          return ()
