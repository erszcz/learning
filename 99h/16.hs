import Test.HUnit

dropEvery xs n = f xs [] []
    where f [] pre acc = reverse (take (n-1) pre ++ acc)
          f (x:xs) pre acc
            | length pre == n = f xs [x] (tail pre ++ acc)
            | otherwise = f xs (x:pre) acc

-- solution - brilliant
{-dropEvery xs n = map fst $ filter ((n/=) . snd) $ zip xs (cycle [1..n])-}

t expected actual = TestCase (assertEqual "" expected actual)

tests = TestList
  [ TestLabel "1" (t [1,1,2,2,3,3] (dropEvery [1,1,1,2,2,2,3,3,3] 3)),
    TestLabel "2" (t [1,2,3,4] (dropEvery [1,1,2,2,3,3,4] 2)) ]

main = do runTestTT tests
          return ()
