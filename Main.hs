module Main where

import Control.Monad.IO.Class ( liftIO )
import System.Console.Haskeline ( InputT, runInputT
                                , Settings(..), defaultSettings
                                , getInputLine )
import Language.Landler ( Step, run )
import System.Environment ( getArgs )
import Text.Interpol ( (^-^) )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] ->
        runInterpreter
    [fn] -> do
         (_, stepss) <- run fn
         putStrLn $ concatMap prettyPrint stepss
    _ ->
        error "Well, fsck.  I didn't think this could happen."

runInterpreter :: IO ()
runInterpreter = printBanner >> runInputT settings loop
    where
      printBanner = putStrLn "landler, version <VERSION>: \
                             \https://github.com/scvalex/landler  \
                             \:? for help"

      settings = defaultSettings { historyFile = Just "~/.landler_history" }

      loop :: InputT IO ()
      loop = do
        minput <- getInputLine "∅ > "
        case minput of
          Nothing    -> return ()
          Just input -> handleStatement input >> loop

      handleStatement :: String -> InputT IO ()
      handleStatement = liftIO . putStrLn

prettyPrint :: [Step] -> String
prettyPrint = unlines . go
    where
      go []            = ["---"]
      go ((t, s) : ts) = (show t) : ("\t" ^-^ s) : go ts
