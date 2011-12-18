{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}

module Language.Landler.Parser (
        Module(..), Statement(..), Term(..), Var, ParseError(..),
        parseModule, parseProgram, parseStatement, parseTerm
    ) where

import Control.Applicative ( (<$>), (<*), (*>), (<*>) )
import qualified Control.Exception as CE
import Control.Monad.Error.Class ( MonadError(..), Error(..) )
import Data.Functor.Identity ( Identity )
import Data.Typeable ( Typeable )
import System.FilePath ( takeFileName )
import Text.Interpol ( (^-^) )
import Text.Parsec ( ParsecT, parse, oneOf, many, many1, manyTill, (<|>), eof
                   , sourceLine, sourceColumn, errorPos )
import qualified Text.Parsec as P
import Text.Parsec.Error ( errorMessages, showErrorMessages )
import Text.Parsec.Language ( emptyDef )
import Text.Parsec.String ( parseFromFile )
import Text.Parsec.Token ( GenLanguageDef(..), LanguageDef
                         , GenTokenParser(..), makeTokenParser )

----------------------------------------------------------------------
-- Types
----------------------------------------------------------------------

-- | Lambda-calculus variable names follow the same rules as Haskell
-- identifiers.  Basically, the first character must be a letter or
-- @_@ and subesequent characters may be letters, numbers and @_@.
type Var = String

-- | Statements are either @let <var> = (<term>)@ which binds the term
-- to the variable name or @(<term>)@ which evaluates the term and
-- prints out the result.  These are meta-syntactic constructs; they
-- are not part of the lambda-calculus.
data Statement = Let Var Term | Call Term | Import String
                 deriving ( Eq )

instance Show Statement where
    show (Let v t)   = "let " ^-^ v ^-^ " = (" ^-^ t ^-^ ")"
    show (Call t)    = "(" ^-^ t ^-^ ")"
    show (Import mn) = "import " ^-^ mn

-- | Lambda-calculus terms are variables, abstractions or
-- applications.
data Term = Var Var | Ab Var Term | App Term Term
            deriving ( Eq )

instance Show Term where
    show (Var v)   = v
    show (Ab v t)  = "\\" ^-^ v ^-^ showAb t
        where
          showAb (Ab v' t') = " " ^-^ v' ^-^ showAb t'
          showAb t'         = ". " ^-^ t'
    show (App t p) = showP t ^-^ " " ^-^ showP p
        where
          showP (Var x) = x
          showP q       = "(" ^-^ q ^-^ ")"

-- | Represents a landler parser error (duh).  It is a thin wrapper
-- around Parsec's @ParseError@.  This exists because we do not want
-- modules above this parser to depend on Parsec.
data ParseError = ParseError Int    -- ^ Line number
                             Int    -- ^ Column number
                             String -- ^ Insightful message
                  deriving ( Eq, Typeable )

instance CE.Exception ParseError

instance Error ParseError where
    strMsg = ParseError 0 0

instance Show ParseError where
    show (ParseError line col msg) =
        let msgs = filter (not . null) $ lines msg
            msg' = unlines $ map ("    " ^-^) msgs
        in "" ^-^ line ^-^ ":" ^-^ col ^-^ ":\n" ^-^ msg'

-- | A 'Module' encapsulates the lambda-data found in a file.
data Module = Module { getModuleName :: String
                     , getModulePath :: FilePath
                     , getModuleImports :: [String]
                     , getModuleBindings :: [(Var, Term)]
                     , getModuleTerms :: [Term]
                     } deriving ( Show )

----------------------------------------------------------------------
-- Parser
----------------------------------------------------------------------

parseModule :: FilePath -> IO Module
parseModule fn = do
  res <- parseFromFile program fn
  case res of
    Left err -> CE.throw (mkParseError err)
    Right ss -> return $ mkModule fn ss

parseProgram :: (MonadError ParseError m) => String -> m [Statement]
parseProgram text = handleResult (parse program "input" text)

parseStatement :: (MonadError ParseError m) => String -> m Statement
parseStatement text = handleResult (parse statement "input" text)

parseTerm :: (MonadError ParseError m) => String -> m Term
parseTerm text = handleResult (parse term "input" text)

handleResult :: (MonadError ParseError m) => Either P.ParseError t -> m t
handleResult res = case res of
                     Left err -> throwError (mkParseError err)
                     Right t  -> return t

program :: LParser [Statement]
program = many1 statement <* eof

statement :: LParser Statement
statement = ws >> (letS <|> importS <|> callS)
    where
      letS = Let <$> (llet *> lvar) <*> (leq *> lparens term)
      importS = Import <$> (limport *> lvar)
      callS = Call <$> lparens term

term :: LParser Term
term = do
  ts <- return . concat =<< many1 terms
  return $ case ts of
             [t] -> t
             _   -> foldl1 App ts

terms :: LParser [Term]
terms = pterms <|> terms'
    where
      pterms = lparens $ (:[]) <$> term
      terms' = do
        m <- ab <|> var
        ns <- many terms
        return (m:concat ns)

ab :: LParser Term
ab = do
  llambda
  vs <- varlist
  t <- term
  return (foldl (flip Ab) t (reverse vs))

varlist :: LParser [String]
varlist = manyTill lvar ldot

var :: LParser Term
var = Var <$> lvar

----------------------------------------------------------------------
-- Lexer
----------------------------------------------------------------------

type LParser a = ParsecT String () Identity a

lambdaCalculusDef :: LanguageDef st
lambdaCalculusDef = emptyDef { commentLine = ";"
                             , opStart = opLetter lambdaCalculusDef
                             , opLetter = oneOf ".\\="
                             , reservedOpNames = [ ".", "\\", "=", "let"
                                                 , "import" ] }

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser lambdaCalculusDef

lvar :: LParser String
lvar = identifier lexer

lparens :: LParser a -> LParser a
lparens = parens lexer

ldot, llambda, llet, limport, leq, ws :: LParser ()
ldot = (reservedOp lexer) "."
llambda = (reservedOp lexer) "\\"
llet = (reserved lexer) "let"
limport = (reserved lexer) "import"
leq = (reservedOp lexer) "="
ws = whiteSpace lexer

----------------------------------------------------------------------
-- Structure
----------------------------------------------------------------------

-- | Convert a Parsec @ParseError@ into the equivalent 'ParseError'.
mkParseError :: P.ParseError -> ParseError
mkParseError err = ParseError (sourceLine $ errorPos err)
                              (sourceColumn $ errorPos err)
                              (showErrorMessages "or" "unknown" "expecting"
                                                 "unexpected" "end of input"
                                                 $ errorMessages err)

-- | Given a list of statements, separate them into imports, bindings,
-- etc. and return a 'Module' structure.
mkModule :: FilePath -> [Statement] -> Module
mkModule fp stmts =
    let (is, bs, ts) = foldr (\s (is1, bs1, ts1) ->
                                  case s of
                                    Let v t -> (is1, (v, t) : bs1, ts1)
                                    Call t  -> (is1, bs1, t : ts1)
                                    Import mn -> (mn : is1, bs1, ts1))
                             ([], [], []) stmts
    in Module { getModuleName = takeFileName fp
              , getModulePath = fp
              , getModuleImports = is
              , getModuleBindings = bs
              , getModuleTerms = ts
              }
