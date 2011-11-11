{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

module Language.Landler (
        -- * Types
        Term(..), Var,

        -- * Reductions
        step, dance,

        -- * Utilities
        subst
    ) where

import Data.Typeable ( Typeable )
import Text.Interpol ( (^-^) )

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
    show (Ab v t)  = "(\\" ^-^ v ^-^ ". " ^-^ t ^-^ ")"
    show (App t p) = "" ^-^ t ^-^ " " ^-^ p

-- | Perform a one-step call-by-name reduction.  Return 'Nothing' if
-- the term is stuck.
step :: Term -> Maybe Term
step (App (Ab x m) n) = Just $ subst m x n
step _                = Nothing

-- | Perform a many-step reduction by calling 'step'
-- repeatedly. Return all the intermediary results (including the
-- original term).
dance :: Term -> [Term]
dance t = case step t of
            Nothing -> [t]
            Just t' -> t : dance t'

-- | Return the substitution in M of the variable X by the term N.
subst :: Term   -- ^ M: The term in which to perform the substitution
      -> Var    -- ^ X: The variable to substitute
      -> Term   -- ^ N: The term with which to substitute X
      -> Term
subst t@(Var y) x n
    | x == y    = n
    | otherwise = t
subst (App m1 m2) x n =
    App (subst m1 x n) (subst m2 x n)
subst t@(Ab y m) x n
    | x == y    = t
    | otherwise = Ab y (subst m x n)
