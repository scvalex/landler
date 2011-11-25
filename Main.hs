module Main where

import Language.Landler ( breakDance )
import Language.Landler.Parser ( Statement(..), parseFile )
import System.Environment ( getArgs )

main :: IO ()
main = do
  [fn] <- getArgs
  stmts <- parseFile fn
  let (terms, binds) = foldr (\s (ts, bs) -> case s of
                                               Let v t -> (ts, (v, t) : bs)
                                               Call t  -> (t : ts, bs))
                             ([], []) stmts
  putStrLn $ concatMap (unlines . map show . breakDance binds) terms
