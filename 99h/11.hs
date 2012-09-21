import Test.HUnit

data Encoded a = Single a
               | Multiple Int a
                 deriving (Show, Eq)

encoded 1 c = Single c
encoded n c = Multiple n c

{-encode (x:xs) = reverse $ (h:t)-}
{-  where (h, t) = foldl f (Single x, []) xs-}
{-        f (Single c, acc) e     = f' 1 c e acc-}
{-        f (Multiple n c, acc) e = f' n c e acc-}
{-        f' n c e acc | c == e    = (encoded (n+1) c, acc)-}
{-                     | otherwise = (encoded 1 e, (encoded n c) : acc)-}

encode xs = h : t
  where (h, t) = foldr f (Single $ last xs, []) $ init xs
        f e (Single c, acc)     = f' 1 c e acc
        f e (Multiple n c, acc) = f' n c e acc
        f' n c e acc | c == e    = (encoded (n+1) c, acc)
                     | otherwise = (encoded 1 e, (encoded n c) : acc)

t expected actual = TestCase (assertEqual "" expected actual)

tests = TestList
  [ TestLabel "1" (t [Multiple 4 'a', Single 'b', Multiple 2 'c',
                      Multiple 2 'a', Single 'd', Multiple 4 'e']
                     (encode "aaaabccaadeeee")) ]

main = do runTestTT tests
          return ()
