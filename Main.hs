{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Exception as CE
import Control.Monad.IO.Class ( MonadIO(..) )
import System.Console.Haskeline ( InputT, runInputT
                                , Settings(..), defaultSettings
                                , getInputLine )
import Language.Landler ( Term, Var
                        , run, breakDance, principalType, principalType'
                        , Statement(..), parseStatement
                        , Error(..) )
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
      handleStatement env plen str = do
        env' <- io $ CE.handle (\(e :: Error) -> do
                            reportError e plen
                            return env) $ do
          let stmt = parseStatement str
          case stmt of
            LetS v t   -> return $ (v, t) : env
            CallS t    -> print (breakDance env t) >>
                          return env
            ImportS _  -> putStrLn "import handling not implemented" >>
                          return env
            TypeS t    -> (CE.handle (\(e :: Error) -> print e) $
                             print (principalType env t)) >>
                          return env
            DeriveS t  -> (CE.handle (\(e :: Error) -> print e) $
                             print (principalType' env t)) >>
                          return env
        return env'

      reportError :: Error -> Int -> IO ()
      reportError err@(ParseError line col _) plen = do
        if line == 1
          then putStrLn $ (replicate (plen + col - 1) ' ') ^-^ "^ here"
          else return ()
        putStrLn $ "Encountered error " ^-^ err
      reportError _ _ = error "cannot encounter other errors"

io :: (MonadIO m) => IO a -> m a
io = liftIO
