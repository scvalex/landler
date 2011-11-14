module Main where

import Language.Landler.Parser ( parseFile )
import System.Environment ( getArgs )

main :: IO ()
main = do
  [fn] <- getArgs
  print =<< parseFile fn
