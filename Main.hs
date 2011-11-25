module Main where

import Language.Landler ( Term, breakDance )
import Language.Landler.Parser ( Statement(..), parseFile )
import System.Environment ( getArgs )
import Text.Interpol ( (^-^) )

main :: IO ()
main = do
  [fn] <- getArgs
  stmts <- parseFile fn
  let (terms, binds) = foldr (\s (ts, bs) -> case s of
                                               Let v t -> (ts, (v, t) : bs)
                                               Call t  -> (t : ts, bs))
                             ([], []) stmts
  putStrLn $ concatMap (prettyPrint . breakDance binds) terms

prettyPrint :: [(Term, String)] -> String
prettyPrint = unlines . go
    where
      go []            = ["---"]
      go ((t, s) : ts) = (show t) : ("\t" ^-^ s) : go ts
