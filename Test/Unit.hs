module Main where

import Language.Landler
import System.Exit ( exitFailure )
import Test.HUnit

main :: IO ()
main = do
  runCounts <- runTestTT $ test [parseTests]
  if failures runCounts + errors runCounts == 0
     then
       putStrLn "All tests pass :)"
     else do
       putStrLn "Failures or errors occured :'("
       exitFailure

parseTests :: Test
parseTests =
    test [ "var" ~: parseTest "x" (v "x")
         , "ab" ~: parseTest "\\x.x" (Ab "x" (v "x"))
         , "app" ~: parseTest "x y" (App (v "x") (v "y"))
         , "app2" ~: parseTest "x y z" (App (App (v "x") (v "y")) (v "z"))
         , "ab-app" ~: parseTest "\\x. yy zz" (Ab "x" (App (v "yy") (v "zz")))
         , "ab2" ~: parseTest "\\x. \\y. x y"
                              (Ab "x" (Ab "y" (App (v "x") (v "y"))))
         , "ab3" ~: parseTest "\\x y. x y"
                              (Ab "x" (Ab "y" (App (v "x") (v "y"))))
         , "ab4" ~: parseTest "\\x y z. z y x"
                              (Ab "x" (Ab "y" (Ab "z"
                                (App (App (v "z") (v "y")) (v "x")))))
         , "app-ab" ~: parseTest "(\\x . x) \\x . x"
                                 (App (Ab "x" (v "x")) (Ab "x" (v "x")))
         , "prec" ~: parseTest "\\x. y \\z. y"
                               (Ab "x" (App (Var "y") (Ab "z" (Var "y"))))
         , "call" ~: parseTestS "(\\x. y)" (Call (Ab "x" (Var "y")))
         , "call2" ~: parseTestS "(\\x. y \\z. y)"
                                 (Call (Ab "x" (App (Var "y")
                                                    (Ab "z" (Var "y")))))
         ]

parseTest :: String -> Term -> Test
parseTest text term = TestCase $ assertEqual "" term (toTerm text)

parseTestS :: String -> Statement -> Test
parseTestS text statement =
    TestCase $ assertEqual "" statement (parseStatement text)

v :: String -> Term
v x = Var x
