{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances #-}

module Language.Landler (
        -- * Types
        Statement(..), Term(..), Var,

        -- * Reading
        parseProgram, parseStatement, ReadTerm(..),

        -- * Reductions
        breakDance, dance, sideStep, step,

        -- * Utilities
        subst, freeVariables, boundVariables
    ) where

import Language.Landler.Parser
import qualified Data.Set as S

-- | Type-class for things that can be turned into 'Term's.
class ReadTerm t where
    toTerm :: t -> Term

instance ReadTerm Term where
    toTerm = id

instance ReadTerm String where
    toTerm = parseTerm

-- | Perform a many-step reduction by 'sideStep' repeatedly.  Return
-- all intermediary results (including the original term).  This is
-- effectively a version of 'dance' that also uses bound names.
breakDance :: (ReadTerm t) => [(Var, Term)] -> t -> [(Term, String)]
breakDance binds rt = let t = toTerm rt
                      in go [t] t
    where
      go soFar t = case sideStep binds t of
                     Left s ->
                         [(t, s)]
                     Right (t', s)
                         | t' `elem` soFar -> [(t, s), (t', "cycling")]
                         | otherwise       -> (t, s) : go (t' : soFar) t'

-- | Perform a many-step reduction by calling 'step' repeatedly.
-- Return all the intermediary results (including the original term).
dance :: (ReadTerm t) => t -> [(Term, String)]
dance rt = let t = toTerm rt
           in go [t] t
    where
      go soFar t = case step t of
                     Left s        -> [(t, s)]
                     Right (t', s)
                         | t' `elem` soFar -> [(t, s), (t', "cycling")]
                         | otherwise       -> (t, s) : go (t' : soFar) t'

-- | Perform a one-step call-by-name reduction with bindings.  Return
-- 'Nothing if the term is stuck.
sideStep :: (ReadTerm t) => [(Var, Term)] -> t -> Either String (Term, String)
sideStep binds = sideStep' . toTerm
    where
      sideStep' (App (Var x) n) = case x `lookup` binds of
                                   Nothing -> Left "no-binding"
                                   Just m  -> Right (App m n, "name-rep")
      sideStep' t               = step t

-- | Perform a one-step call-by-name reduction.  Return 'Nothing' if
-- the term is stuck.
step :: (ReadTerm t) => t -> Either String (Term, String)
step = step' . toTerm
    where
      step' (App (Ab x m) n) = Right (subst m x n, "subst")
      step' _                = Left "stuck"

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
    | x == y           = t
    | y `S.member` fvn = let z = newVar usedVariables
                         in subst (Ab z (subst m y (Var z))) x n
    | otherwise        = Ab y (subst m x n)
    where
      fvn = freeVariables n
      usedVariables = S.unions [ fvn, freeVariables n, boundVariables m
                               , boundVariables n ]

-- | Return the set of free variables in the given term.
freeVariables :: Term -> S.Set Var
freeVariables (Var x)     = S.singleton x
freeVariables (App m1 m2) = freeVariables m1 `S.union` freeVariables m2
freeVariables (Ab y m)    = y `S.delete` freeVariables m

-- | Return the set of bound variables in the given term.
boundVariables :: Term -> S.Set Var
boundVariables (Var _)     = S.empty
boundVariables (App m1 m2) = boundVariables m1 `S.union` boundVariables m2
boundVariables (Ab y m)    = y `S.insert` boundVariables m

-- | A lot of variable names.
allVars :: [Var]
allVars = let vs = "" : [v ++ [s] | v <- vs, s <- ['a'..'z']]
          in tail vs

-- | Return a variable name not found in the given set.
newVar :: S.Set Var -> Var
newVar usedVariables = head $ dropWhile (flip S.member usedVariables) allVars
