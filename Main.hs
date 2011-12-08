module Main where

import Control.Monad.IO.Class ( liftIO )
import Data.Maybe ( fromJust )
import System.Console.Haskeline ( InputT, runInputT
                                , Settings(..), defaultSettings
                                , getInputLine )
import Language.Landler ( Term, Var
                        , Step, run, breakDance
                        , Statement(..), parseStatement
                        , ParseError(..) )
import System.Environment ( getArgs )
import Text.Interpol ( (^-^) )

type Environment = [(Var, Term)]

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
        error "FIXME Use cmdargs or something."

runInterpreter :: IO ()
runInterpreter = printBanner >> runInputT settings (loop [])
    where
      printBanner = putStrLn "landler, version <VERSION>: \
                             \https://github.com/scvalex/landler  \
                             \:? for help"

      settings = defaultSettings { historyFile = Just "~/.landler_history" }

      loop :: Environment -> InputT IO ()
      loop env = do
        let envKeys = case unwords $ map fst env of
                        "" -> "∅"
                        ek -> ek
        minput <- getInputLine (envKeys ^-^ " > ")
        case minput of
          Nothing    -> return ()
          Just input -> handleStatement env input >>= loop

      handleStatement :: Environment -> String -> InputT IO Environment
      handleStatement env str =
          case parseStatement str of
            Left err -> do
              liftIO $ reportError err str
              return env
            Right stmt -> do
              case stmt of
                Let v t -> return $ (v, t) : env
                Call t  -> liftIO (putStrLn . prettyPrint . fromJust $
                                   breakDance env t) >>
                           return env

      reportError :: ParseError -> String -> IO ()
      reportError err _ = putStrLn $ "Encountered error " ^-^ err

prettyPrint :: [Step] -> String
prettyPrint = unlines . go
    where
      go []            = ["---"]
      go ((t, s) : ts) = (show t) : ("\t" ^-^ s) : go ts
