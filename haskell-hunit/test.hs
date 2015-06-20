import Test.HUnit

foo _ = (1,2)
partA _ = do return (5,5)
partB _ = do return True

test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))
test2 = TestCase (do (x,y) <- partA 3
                     assertEqual "for the first result of partA," 5 x
                     b <- partB y
                     assertBool ("(partB " ++ show y ++ ") failed") b)

tests = TestList [TestLabel "test1" test1,
                  TestLabel "test2" test2]

main = do runTestTT tests
          return ()
