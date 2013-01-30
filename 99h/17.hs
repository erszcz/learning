import Test.HUnit

import Data.List as L

-- using L.partition and zipping with infinite list
-- split xs n = (map fst l, map fst g) where
--     (l, g) = L.partition ((<= n) . snd) (zip xs [1..])

-- direct approach
-- split xs n = split' xs [] n where
--     split' xs acc 0 = (reverse acc, xs)
--     split' (x:xs) acc n = split' xs (x:acc) (n-1)

-- direct approach wo accumulation / non tail-recursive
split xs     0 = ([], xs)
split (x:xs) n = let (f,l) = split xs (n-1) in (x:f, l)

t expected actual = TestCase (assertEqual "" expected actual)

tests = TestList
  [ TestLabel "1" (t ([1,1,2], [2,2,3,3,3]) (split [1,1,2,2,2,3,3,3] 3)) ]

main = do runTestTT tests
          return ()
