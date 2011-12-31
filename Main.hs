{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Exception as CE
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Maybe ( fromJust )
import System.Console.Haskeline ( InputT, runInputT
                                , Settings(..), defaultSettings
                                , getInputLine )
import Language.Landler ( Term, Var
                        , run, breakDance, typ3
                        , Statement(..), parseStatement
                        , Error(..), ParseError(..) )
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
         putStrLn $ concatMap show stepss
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
              io $ reportError err plen
              return env
            Right stmt -> do
              case stmt of
                LetS v t   -> return $ (v, t) : env
                CallS t    -> io (print . fromJust $ breakDance env t) >>
                              return env
                ImportS _  -> io $ putStrLn "import handling not \
                                            \implemented" >>
                              return env
                TypeS t -> io (CE.handle (\(e :: Error) -> print e) $
                               print . fromJust $ typ3 env t) >>
                           return env

      reportError :: ParseError -> Int -> IO ()
      reportError err@(ParseError line col _) plen = do
        if line == 1
          then putStrLn $ (replicate (plen + col - 1) ' ') ^-^ "^ here"
          else return ()
        putStrLn $ "Encountered error " ^-^ err

io :: (MonadIO m) => IO a -> m a
io = liftIO
