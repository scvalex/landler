{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

module Language.Landler (
        Term(..), Var,
        step, dance
    ) where

import Data.Typeable ( Typeable )
import Text.Interpol ( (^-^) )

type Var = String

data Term = Var Var | Ab Var Term | App Term Term
          deriving ( Eq, Typeable )

instance Show Term where
    show (Var v)   = v
    show (Ab v t)  = "(\\" ^-^ v ^-^ ". " ^-^ t ^-^ ")"
    show (App t p) = "" ^-^ t ^-^ " " ^-^ p

step :: Term -> Maybe Term
step (App (Ab vx vm) vn) = Just $ repl vm vx vn
    where
      repl :: Term -> Var -> Term -> Term
      repl t@(Ab y m) x n
          | x == y    = t
          | otherwise = Ab y (repl m x n)
      repl (App t p) x n = App (repl t x n) (repl p x n)
      repl t@(Var x) y n
          | x == y    = n
          | otherwise = t
step _ = Nothing

dance :: Term -> [Term]
dance t = case step t of
            Nothing -> [t]
            Just t' -> t : dance t'
