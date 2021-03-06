{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Exception as CE
import Language.Landler
import System.Exit ( exitFailure )
import Test.HUnit
import Text.Interpol

main :: IO ()
main = do
  runCounts <- runTestTT $ test [parseTests, exampleTests, typeTests]
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
    test . concat $
         [ map (\(n, fp, ts) -> n ~: exampleTest fp ts) positiveExampleTests
         , map (\(n, s) -> n ~: negExampleTest s) negativeExampleTests ]

typeTests :: Test
typeTests = test $ map (\(n, t, ty) -> n ~: typeTest t ty) positiveTypeTests

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
    [ ("call", "(\\x. y)", CallS (Ab "x" (Var "y")))
    , ("call2", "(\\x. y \\z. y)",
       CallS (Ab "x" (App (Var "y") (Ab "z" (Var "y")))))
    , ("let", "let id = (\\x. x)", LetS "id" (Ab "x" (Var "x")))
    , ("let2", "let m = (\\x y z. z y x)",
       LetS "m" (Ab "x" (Ab "y" (Ab "z" (App (App (v "z") (v "y")) (v "x"))))))
    , ("import", "import prelude", ImportS "prelude")
    ]

positiveExampleTests :: [(String, FilePath, [Term])]
positiveExampleTests =
    [ ("pair", "examples/pair.lambda", [v "m", v "n"])
    , ("cond", "examples/cond.lambda", [v "m", v "n"])
    , ("scott", "examples/scott.lambda",
       [ v "true", v "false", v "true", v "true", v "true"
       , v "true", v "false", v "false" ])
    , ("import", "examples/import.lambda", [ v "y" ])
    ]

negativeExampleTests :: [(String, FilePath)]
negativeExampleTests =
    [ ("import-cycle1", "examples/cycleA.lambda")
    , ("import-cycle2", "examples/cycleB.lambda")
    ]

positiveTypeTests :: [(String, String, Type)]
positiveTypeTests =
    [ ("type-I", "(\\x. x)", TypeArr (tv "a") (tv "a"))
    , ("type-K", "(\\x y. x)",
       TypeArr (tv "a") (TypeArr (tv "b") (tv "a")))
    , ("type-S", "(\\x y z. x z (y z))",
       TypeArr (TypeArr (tv "a") (TypeArr (tv "b") (tv "c")))
               (TypeArr (TypeArr (tv "a") (tv "b"))
                        (TypeArr (tv "a") (tv "c"))))
    , ("type-K2", "(\\b c. c)", TypeArr (tv "a") (TypeArr (tv "b") (tv "b")))
    , ("type-v", "(\\b c. (\\y. c) (b c))",
       TypeArr (TypeArr (tv "b") (tv "a")) (TypeArr (tv "b") (tv "b")))
    , ("type-vi", "(\\b c. (\\x y. x) c (b c))",
       TypeArr (TypeArr (tv "b") (tv "a")) (TypeArr (tv "b") (tv "b")))
    , ("type-vii", "((\\a b c. a c (b c)) (\\x y. x))",
       TypeArr (TypeArr (tv "b") (tv "a")) (TypeArr (tv "b") (tv "b")))
    ]

parseTest :: String -> Term -> Test
parseTest text term = TestCase $ assertEqual "" term (toTerm text)

negParseTest :: String -> Test
negParseTest text = TestCase $ CE.handle (\(_ :: Error) -> return ()) $ do
                      let t = toTerm text
                      t `seq` assertFailure ""

parseTestS :: String -> Statement -> Test
parseTestS text statement = let res = parseStatement text
                            in TestCase $ assertEqual "" statement res

exampleTest :: FilePath -> [Term] -> Test
exampleTest fp terms = TestCase $ do
                         (_, stepss) <- run fp
                         assertEqual "" terms (map (fst . last . fromCallR)
                                                   stepss)
    where
      fromCallR (CallR steps) = steps
      fromCallR r             = error $ "unexpected result " ^-^ r

negExampleTest :: FilePath -> Test
negExampleTest fp = TestCase $
                    CE.handle (\(_ :: CE.SomeException) -> return ()) $
                    run fp >> return ()

typeTest :: String -> Type -> Test
typeTest rt ty = TestCase $ do
                   let ty1 = principalType [] rt
                   assertEqual "" (canonicalForm ty) (canonicalForm ty1)

v :: String -> Term
v x = Var x

tv :: String -> Type
tv x = TypeVar x
