import Test.HUnit

repli l n = concatMap (replicate n) l

t expected actual = TestCase (assertEqual "" expected actual)

tests = TestList
  [ TestLabel "1" (t [1,1,1,2,2,2,3,3,3]
                     (repli [1,2,3] 3)) ]

main = do runTestTT tests
          return ()
