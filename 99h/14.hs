import Test.HUnit

dupli = concatMap (\e -> [e,e])

t expected actual = TestCase (assertEqual "" expected actual)

tests = TestList
  [ TestLabel "1" (t [1,1,2,2,3,3]
                     (dupli [1,2,3])) ]

main = do runTestTT tests
          return ()
