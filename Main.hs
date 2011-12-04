module Main where

import Language.Landler ( Step, run )
import System.Environment ( getArgs )
import Text.Interpol ( (^-^) )

main :: IO ()
main = do
  [fn] <- getArgs
  (_, stepss) <- run fn
  putStrLn $ concatMap prettyPrint stepss

prettyPrint :: [Step] -> String
prettyPrint = unlines . go
    where
      go []            = ["---"]
      go ((t, s) : ts) = (show t) : ("\t" ^-^ s) : go ts
