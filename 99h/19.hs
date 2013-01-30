import Test.HUnit

-- rotate xs n | n > 0 = (drop n xs) ++ (take n xs)
-- rotate xs n | n < 0 = reverse $ rotate (reverse xs) (-n)

rotate xs n = (drop l xs) ++ (take l xs)
    where l | n >= 0 = n
            | n <  0 = length xs + n

t expected actual = TestCase (assertEqual "" expected actual)

tests = TestList
  [ TestLabel "1"
        (t ("defghabc")
        (rotate ['a','b','c','d','e','f','g','h'] 3)),
    TestLabel "2"
        (t ("ghabcdef")
        (rotate ['a','b','c','d','e','f','g','h'] (-2))) ]

main = do runTestTT tests
          return ()
