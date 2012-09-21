import Data.List (concat, unfoldr)
import Test.HUnit

data Encoded a = Single a
               | Multiple Int a
                 deriving (Show, Eq)

encoded 1 c = Single c
encoded n c = Multiple n c

{-decode :: [Encoded a] -> [a]-}
{-decode = concat . unfoldr decode'-}
{-  where decode' []                 = Nothing-}
{-        decode' ((Single a):t)     = Just ([a], t)-}
{-        decode' ((Multiple n a):t) = Just (take n $ repeat a, t)-}

-- How about not using concat?
decode :: [Encoded a] -> [a]
decode = foldr decode' []
  where decode' (Single a)     acc = a:acc
        -- I think this is equivalent to decode variant with concat
        -- as the partial lists are still iterated over for concatenation.
        {-decode' (Multiple n a) acc = (take n $ repeat a) ++ acc-}
        -- But this is not, I think. Whether it's worse or better
        -- is a horse of another colour, though.
        decode' (Multiple n a) acc = decode' (encoded (n-1) a) (a:acc)

-- Solution (ignores reiterating over the result list to concat it)
{-toTuple :: Encoded a -> (Int, a)-}
{-toTuple (Single x)     = (1, x)-}
{-toTuple (Multiple n x) = (n, x)-}

{-decode :: [Encoded a] -> [a]-}
{-decode = concatMap (uncurry replicate . toTuple)-}

t expected actual = TestCase (assertEqual "" expected actual)

tests = TestList
  [ TestLabel "1" (t "aaaabccaadeeee"
                     (decode [Multiple 4 'a', Single 'b', Multiple 2 'c',
                              Multiple 2 'a', Single 'd', Multiple 4 'e'])) ]

main = do runTestTT tests
          return ()
