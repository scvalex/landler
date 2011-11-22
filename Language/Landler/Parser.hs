{-# LANGUAGE DeriveDataTypeable #-}

module Language.Landler.Parser (
        Term(..), Var,
        parseStatement, parseTerm, parseFile
    ) where

import Control.Applicative ( (<$>), (*>), (<*>) )
import Data.Functor.Identity ( Identity )
import Text.Interpol ( (^-^) )
import Text.Parsec ( ParsecT, parse, oneOf, many, many1, manyTill, (<|>) )
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
data Statement = Let Var Term | Call Term

instance Show Statement where
    show (Let v t) = "let " ^-^ v ^-^ " = (" ^-^ t ^-^ ")"
    show (Call t)  = "(" ^-^ t ^-^ ")"

-- | Lambda-calculus terms are variables, abstractions or
-- applications.
data Term = Var Var | Ab Var Term | App Term Term

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

----------------------------------------------------------------------
-- Parser
----------------------------------------------------------------------

parseFile :: FilePath -> IO [Statement]
parseFile fn = do
  res <- parseFromFile (many1 statement) fn
  case res of
    Left err  -> error (show err)
    Right sts -> return sts

parseStatement :: String -> Statement
parseStatement text = case parse statement "input" text of
                        Left err -> error (show err)
                        Right t  -> t

parseTerm :: String -> Term
parseTerm text = case parse term "input" text of
                   Left err -> error (show err)
                   Right t  -> t

statement :: LParser Statement
statement = letS <|> callS
    where
      letS :: LParser Statement
      letS = Let <$> (llet *> lvar) <*> (leq *> lparens term)

      callS :: LParser Statement
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
lambdaCalculusDef = emptyDef { commentLine = "#"
                             , opStart = opLetter lambdaCalculusDef
                             , opLetter = oneOf ".\\="
                             , reservedOpNames = [".", "\\", "=", "let"] }

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser lambdaCalculusDef

lvar :: LParser String
lvar = identifier lexer

lparens :: LParser a -> LParser a
lparens = parens lexer

ldot :: LParser ()
ldot = (reservedOp lexer) "."

llambda :: LParser ()
llambda = (reservedOp lexer) "\\"

llet :: LParser ()
llet = (reserved lexer) "let"

leq :: LParser ()
leq = (reservedOp lexer) "="
