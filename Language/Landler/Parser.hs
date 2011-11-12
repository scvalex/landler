{-# LANGUAGE DeriveDataTypeable #-}

module Language.Landler.Parser (
        Term(..), Var,
        parseTerm
    ) where

import Control.Applicative ( (<$>), (*>), (<*>) )
import Data.Typeable ( Typeable )
import Text.Interpol ( (^-^) )
import Text.Parsec ( Parsec, parse, oneOf, many, many1, (<|>) )
import Text.Parsec.Language ( emptyDef )
import Text.Parsec.Token ( GenLanguageDef(..), LanguageDef
                         , GenTokenParser(..), makeTokenParser )

----------------------------------------------------------------------
-- Types
----------------------------------------------------------------------

-- | Lambda-calculus variable names follow the same rules as Haskell
-- identifiers.  Basically, the first character must be a letter or
-- @_@ and subesequent characters may be letters, numbers and @_@.
type Var = String

-- | Lambda-calculus terms are variables, abstractions or
-- applications.
data Term = Var Var | Ab Var Term | App Term Term
          deriving ( Eq, Typeable )

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

parseTerm :: String -> Term
parseTerm text = case parse term "input" text of
                   Left err -> error (show err)
                   Right t  -> t

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
ab = Ab <$> (llambda *> lvar) <*> (ldot *> term)

var :: LParser Term
var = Var <$> lvar

----------------------------------------------------------------------
-- Lexer
----------------------------------------------------------------------

type LParser a = Parsec String () a

lambdaCalculusDef :: LanguageDef st
lambdaCalculusDef = emptyDef { commentLine = "#"
                             , opStart = opLetter lambdaCalculusDef
                             , opLetter = oneOf ".\\"
                             , reservedOpNames = [".", "\\"] }

lvar :: LParser String
lop :: String -> LParser ()
lparens :: LParser a -> LParser a
skipws :: LParser ()

TokenParser { identifier = lvar
            , reservedOp = lop
            , parens = lparens
            , whiteSpace = skipws
            } = makeTokenParser lambdaCalculusDef

ldot :: LParser ()
ldot = lop "."

llambda :: LParser ()
llambda = lop "\\"
