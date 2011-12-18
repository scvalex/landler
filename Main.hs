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
        let prompt = envKeys ^-^ " > "
        minput <- getInputLine prompt
        case minput of
          Nothing    -> return ()
          Just input -> handleStatement env (length prompt) input >>= loop

      handleStatement :: Environment -> Int -> String -> InputT IO Environment
      handleStatement env plen str =
          case parseStatement str of
            Left err -> do
              liftIO $ reportError err plen
              return env
            Right stmt -> do
              case stmt of
                Let v t   -> return $ (v, t) : env
                Call t    -> liftIO (putStrLn . prettyPrint . fromJust $
                                     breakDance env t) >>
                             return env
                Import _  -> liftIO $ putStrLn "import handling not \
                                               \implemented" >>
                             return env

      reportError :: ParseError -> Int -> IO ()
      reportError err@(ParseError line col _) plen = do
        if line == 1
          then putStrLn $ (replicate (plen + col - 1) ' ') ^-^ "^ here"
          else return ()
        putStrLn $ "Encountered error " ^-^ err

prettyPrint :: [Step] -> String
prettyPrint = unlines . go
    where
      go []            = ["---"]
      go ((t, s) : ts) = (show t) : ("\t" ^-^ s) : go ts
