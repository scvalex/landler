module Main where

import Data.Maybe
import Language.Landler
import System.Exit ( exitFailure )
import Test.HUnit

main :: IO ()
main = do
  runCounts <- runTestTT $ test [parseTests, exampleTests]
  if failures runCounts + errors runCounts == 0
     then
       putStrLn "All tests pass :)"
     else do
       putStrLn "Failures or errors occured :'("
       exitFailure

parseTests :: Test
parseTests =
    test . concat $
             [ map (\(n, s, t) -> n ~: parseTest s t) positiveTermParses
             , map (\(n, s) -> n ~: negParseTest s) negativeTermParses
             , map (\(n, s, t) -> n ~: parseTestS s t) positiveStatementParses
             ]

exampleTests :: Test
exampleTests =
    test $ map (\(n, fp, ts) -> n ~: exampleTest fp ts) exampleResults

positiveTermParses :: [(String, String, Term)]
positiveTermParses =
    [ ("var", "x", v "x")
    , ("ab", "\\x.x", Ab "x" (v "x"))
    , ("app", "x y", App (v "x") (v "y"))
    , ("app2", "x y z", App (App (v "x") (v "y")) (v "z"))
    , ("ab-app", "\\x. yy zz", Ab "x" (App (v "yy") (v "zz")))
    , ("ab2", "\\x. \\y. x y", Ab "x" (Ab "y" (App (v "x") (v "y"))))
    , ("ab3", "\\x y. x y", Ab "x" (Ab "y" (App (v "x") (v "y"))))
    , ("ab4", "\\x y z. z y x",
       Ab "x" (Ab "y" (Ab "z" (App (App (v "z") (v "y")) (v "x")))))
    , ("app-ab", "(\\x . x) \\x . x", App (Ab "x" (v "x")) (Ab "x" (v "x")))
    , ("prec", "\\x. y \\z. y", Ab "x" (App (Var "y") (Ab "z" (Var "y"))))
    ]

negativeTermParses :: [(String, String)]
negativeTermParses =
    [ ("no-body", "\\x")
    , ("no-head", "\\.x")
    , ("emptyab", "\\.")
    , ("inv-var", "12x")
--    , ("dots", "\\x. y. z")  We can't check for trailling garbage.
--    , ("dots2", "\\x. y.")
    , ("slshs", "\\\\")
    , ("slshs2", "\\x.\\\\")
    ]

positiveStatementParses :: [(String, String, Statement)]
positiveStatementParses =
    [ ("call", "(\\x. y)", Call (Ab "x" (Var "y")))
    , ("call2", "(\\x. y \\z. y)",
       Call (Ab "x" (App (Var "y") (Ab "z" (Var "y")))))
    , ("let", "let id = (\\x. x)", Let "id" (Ab "x" (Var "x")))
    , ("let2", "let m = (\\x y z. z y x)",
       Let "m" (Ab "x" (Ab "y" (Ab "z" (App (App (v "z") (v "y")) (v "x"))))))
    ]

exampleResults :: [(String, FilePath, [Term])]
exampleResults =
    [ ("pair", "examples/pair.lambda", [v "m", v "n"])
    , ("cond", "examples/cond.lambda", [v "m", v "n"])
    , ("scott", "examples/scott.lambda",
       [ v "true", v "false", v "true", v "true", v "true"
       , v "true", v "false", v "false" ])
    ]

parseTest :: String -> Term -> Test
parseTest text term = TestCase $ assertEqual "" term (fromJust $ toTerm text)

negParseTest :: String -> Test
negParseTest text = TestCase $ assertEqual "" Nothing (toTerm text)

parseTestS :: String -> Statement -> Test
parseTestS text statement = let (Right res) = parseStatement text
                            in TestCase $ assertEqual "" statement res

exampleTest :: FilePath -> [Term] -> Test
exampleTest fp terms = TestCase $ do
                         (_, stepss) <- run fp
                         assertEqual "" terms (map (fst . last) stepss)

v :: String -> Term
v x = Var x
