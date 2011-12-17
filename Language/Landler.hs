{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances #-}

module Language.Landler (
        -- * Types
        Statement(..), Environment, Step, Term(..), Var,

        -- * Reading
        parseProgram, parseStatement, parseTerm, ReadTerm(..),
        ParseError(..),

        -- * Reductions
        run, breakDance, dance, sideStep, step,

        -- * Utilities
        subst, freeVariables, boundVariables
    ) where

import Control.Monad ( when )
import qualified Data.Set as S
import Language.Landler.Parser
import System.FilePath ( replaceBaseName )
import Text.Interpol ( (^-^) )

-- | Type-class for things that can be turned into 'Term's.
class ReadTerm t where
    toTerm :: (Monad m) => t -> m Term

instance ReadTerm Term where
    toTerm = return

instance ReadTerm String where
    toTerm str = case parseTerm str of
                   Left err -> fail ("error " ^-^ err)
                   Right t  -> return t

-- | An 'Environment' maps names to the terms they represent.
type Environment = [(Var, Term)]

-- | A 'Step' is a term and a description of the reduction (if any)
-- that can be applied to it.
type Step = (Term, String)

-- | Run the LC program in the given file and return the bound names
-- and the sequences of steps that in the evaluations of the calls
-- read.
run :: FilePath -> IO (Environment, [[Step]])
run fn = do
  (terms, binds) <- parseFileResolveImports [] fn
  stepss <- mapM (breakDance binds) terms
  return (binds, stepss)

-- | Perform a many-step reduction by 'sideStep' repeatedly.  Return
-- all intermediary results (including the original term).  This is
-- effectively a version of 'dance' that also uses bound names.  The
-- only way to cause this function to fail is to pass it a parameter
-- that cannot be converted to a term.
breakDance :: (ReadTerm t, Monad m) => Environment -> t -> m [Step]
breakDance binds rt = do
  t <- toTerm rt
  return $ go [t] t
    where
      go soFar t = case sideStep binds t of
                     Left s ->
                         [(t, s)]
                     Right (t', s)
                         | t' `elem` soFar -> [(t, s), (t', "cycling")]
                         | otherwise       -> (t, s) : go (t' : soFar) t'

-- | Perform a many-step reduction by calling 'step' repeatedly.
-- Return all the intermediary results (including the original term).
-- The only way to cause this function to fail is to pass it a
-- parameter that cannot be converted to a term.
dance :: (ReadTerm t, Monad m) => t -> m [Step]
dance = breakDance []

-- | Perform a one-step call-by-name reduction with bindings.  Return
-- 'Nothing if the term is stuck.
sideStep :: Environment -> Term -> Either String Step
sideStep binds (App (Var x) n) = case x `lookup` binds of
                                   Nothing -> Left "no-binding"
                                   Just m  -> Right (App m n, "name-rep")
sideStep binds t               = step' (sideStep binds) t

-- | Perform a one-step call-by-name reduction.  Return 'Nothing' if
-- the term is stuck.
step :: Term -> Either String Step
step = step' step

-- | Perform a one-step call-by-name reduction.  If the top-level of
-- an application cannot be reduced, use REDUCER to reduce the LHS.
step' :: (Term -> Either String Step) -- ^ REDUCER to call
                                      -- for nested
                                      -- reductions
      -> Term                         -- ^ TERM to reduce
      -> Either String Step
step' _ (App (Ab x m) n) = Right (subst m x n, "subst")
step' reducer (App m n)  = case reducer m of
                             Left reason ->
                                 Left $ "LHS not reducible: " ^-^ reason
                             Right (m', s) ->
                                 Right (App m' n, s)
step' _ _                = Left "stuck: top-level is not an application"

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

-- | Parse the specified file and resolve any imports it may have by
-- parsing and resolving those files as well.  Return a pair
-- containing the list of terms read and the list of bindings found.
-- Terms and bindings from imported modules appear before terms and
-- bindings from the importing modules.
parseFileResolveImports :: [FilePath] -- ^ List of files already
                                      -- loaded.  If a cycle is
                                      -- detected, fail.
                        -> FilePath
                        -> IO ([Term], [(Var, Term)])
parseFileResolveImports fns fn = do
  when (fn `elem` fns) . fail $ "import cycle detected: " ^-^ fns
  m <- parseModule fn
  resolves <- mapM (parseFileResolveImports (fn:fns) . findModule fn)
                   (getModuleImports m)
  let (extraTerms, extraBinds) =
          foldl (\(ets, ebs) (ts, bs) -> (ets ++ ts, ebs ++ bs))
                ([], []) resolves
  return ( extraTerms ++ (getModuleTerms m)
         , extraBinds ++ (getModuleBindings m))
    where
      findModule :: FilePath -> String -> FilePath
      findModule = replaceBaseName
