{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, FlexibleContexts,
             TypeSynonymInstances #-}

module Language.Landler.Parser (
        ReadTerm(..),
        parseModule, parseStatement, parseTerm
    ) where

import Control.Applicative ( (<$>), (<*), (*>), (<*>) )
import qualified Control.Exception as CE
import Data.Functor.Identity ( Identity )
import Language.Landler.Types ( Module(..), Statement(..), Term(..)
                              , Error(..) )
import System.FilePath ( takeFileName )
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

-- | Type-class for things that can be turned into 'Term's.  Note
-- that, as currently defined, 'toTerm' may throw 'ParseError'
-- exceptions.
class ReadTerm t where
    toTerm :: t -> Term

instance ReadTerm Term where
    toTerm = id

instance ReadTerm String where
    toTerm = parseTerm

----------------------------------------------------------------------
-- Parser
----------------------------------------------------------------------

parseModule :: FilePath -> IO Module
parseModule fn = do
  res <- parseFromFile program fn
  case res of
    Left err -> CE.throw (mkParseError err)
    Right ss -> return $ mkModule fn ss

parseStatement :: String -> Statement
parseStatement text = handleResult (parse statement "input" text)

parseTerm :: String -> Term
parseTerm text = handleResult (parse term "input" text)

handleResult :: Either P.ParseError t -> t
handleResult res = case res of
                     Left err -> CE.throw (mkParseError err)
                     Right t  -> t

program :: LParser [Statement]
program = many1 statement <* eof

statement :: LParser Statement
statement = ws >> (letS <|> importS <|> callS <|> typeS <|> deriveS)
    where
      letS = LetS <$> (llet *> lvar) <*> (leq *> lparens term)
      importS = ImportS <$> (limport *> lvar)
      callS = CallS <$> lparens term
      typeS = TypeS <$> (ltype *> lparens term)
      deriveS = DeriveS <$> (lderive *> lparens term)

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
                                                 , "import", "type", "derive" ] }

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser lambdaCalculusDef

lvar :: LParser String
lvar = identifier lexer

lparens :: LParser a -> LParser a
lparens = parens lexer

ldot, llambda, llet, limport, ltype, lderive, leq, ws :: LParser ()
ldot = (reservedOp lexer) "."
llambda = (reservedOp lexer) "\\"
llet = (reserved lexer) "let"
limport = (reserved lexer) "import"
ltype = (reserved lexer) "type"
lderive = (reserved lexer) "derive"
leq = (reservedOp lexer) "="
ws = whiteSpace lexer

----------------------------------------------------------------------
-- Structure
----------------------------------------------------------------------

-- | Convert a Parsec @ParseError@ into the equivalent 'ParseError'.
mkParseError :: P.ParseError -> Error
mkParseError err = ParseError (sourceLine $ errorPos err)
                              (sourceColumn $ errorPos err)
                              (showErrorMessages "or" "unknown" "expecting"
                                                 "unexpected" "end of input"
                                                 $ errorMessages err)

-- | Given a list of statements, separate them into imports, bindings,
-- etc. and return a 'Module' structure.
mkModule :: FilePath -> [Statement] -> Module
mkModule fp stmts =
    let m = Module { getModuleName    = takeFileName fp
                   , getModulePath    = fp
                   , getModuleImports = []
                   , getModuleLets    = []
                   , getModuleCalls   = []
                   , getModuleTypes   = []
                   , getModuleDerives = []
                   }
    in foldr (\s m' -> case s of
                         LetS v t ->
                             m' { getModuleLets = (v, t) : getModuleLets m' }
                         CallS t ->
                             m' { getModuleCalls = t : getModuleCalls m' }
                         ImportS mn ->
                             m' { getModuleImports =
                                      mn : getModuleImports m' }
                         TypeS t ->
                             m' { getModuleTypes = t : getModuleTypes m' }
                         DeriveS t ->
                             m' { getModuleDerives = t : getModuleDerives m' })
             m stmts
