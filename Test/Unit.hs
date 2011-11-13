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
    test [ "var" ~: parseTest "x" (Var "x")
         , "ab" ~: parseTest "\\x.x" (Ab "x" (Var "x"))
         , "app" ~: parseTest "x y" (App (Var "x") (Var "y"))
         , "app2" ~: parseTest "x y z" (App (App (Var "x") (Var "y")) (Var "z"))
         , "ab-app" ~: parseTest "\\x. yy zz" (Ab "x" (App (Var "yy") (Var "zz")))
         , "ab2" ~: parseTest "\\x. \\y. x y"
                              (Ab "x" (Ab "y" (App (Var "x") (Var "y"))))
         , "ab3" ~: parseTest "\\x y. x y" (Ab "x" (Ab "y" (App (Var "x") (Var "y"))))
         ]

parseTest :: String -> Term -> Test
parseTest text term = TestCase $ assertEqual "" (toTerm text) term
