{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances #-}

module Language.Landler (
        -- * Types
        Statement(..), Environment, Result(..), Step, Term(..), Var,

        -- * Reading
        parseProgram, parseStatement, parseTerm, ReadTerm(..),
        ParseError(..),

        -- * Reductions
        run, breakDance, dance, sideStep, step,

        -- * Typing
        typ3,

        -- * Utilities
        subst, freeVariables, boundVariables
    ) where

import Control.Monad ( when )
import Data.Char ( toLower, toUpper )
import qualified Data.Set as S
import qualified Data.Map as M
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

-- | The result of executing a 'Statement'.
data Result = CallR [Step]
            | TypeR Type

instance Show Result where
    show (CallR steps) = unlines $ go steps
        where
          go []            = ["---"]
          go ((t, s) : ts) = (show t) : ("\t" ^-^ s) : go ts
    show (TypeR typ) = show typ

-- | A 'Step' is a term and a description of the reduction (if any)
-- that can be applied to it.
type Step = (Term, String)

data Type = TypeVar Var
          | TypeArr Type Type
          | Untypeable
            deriving ( Eq )

instance Show Type where
    show (TypeVar v) = v
    show (TypeArr (TypeVar v) t) = v ^-^ " → " ^-^ t
    show (TypeArr t1 t2) = "(" ^-^ t1 ^-^ ") → " ^-^ t2
    show Untypeable = "Untypeable"

-- | Run the LC program in the given file and return the bound names
-- and the sequences of steps that in the evaluations of the calls
-- read.
run :: FilePath -> IO (Environment, [Result])
run fn = do
  Module { getModuleCalls = calls
         , getModuleLets  = lets
         , getModuleTypes = types } <- parseModuleResolveImports [] fn
  callResults <- mapM (breakDance lets) calls
  typeResults <- mapM (typ3 lets) types
  return (lets, map CallR callResults ++ map TypeR typeResults)

-- | Determine the type for the given term, taking into account the
-- given environment.
typ3 :: (ReadTerm t, Monad m) => Environment -> t -> m Type
typ3 _ rt = do
  t <- toTerm rt
  return . fst $ go M.empty t
    where
      go env (Var v)  = let t' = fresh env
                        in case M.lookup v env of
                             Nothing -> (t', M.insert v t' env)
                             Just t  -> (t, env)
      go env (Ab v t) = let t1 = fresh env
                            env' = M.insert v t1 env
                            (t2, env'') = go env' t
                        in (TypeArr t1 t2, M.delete v env'')
      go env _        = (Untypeable, env)

      fresh :: M.Map Var Type -> Type
      fresh env = let usedTypes = map (map toLower) .
                                  concatMap fromTypeVar $ M.elems env
                  in TypeVar . map toUpper . newVar $ S.fromList usedTypes

      fromTypeVar (TypeVar v) = [v]
      fromTypeVar _           = []

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

-- | Parse the specified module and resolve any imports it may have by
-- parsing and resolving those modules as well.  Return a 'Module'
-- with the meta-data corresponding to the target module, the various
-- content entries expanded as necessary and the import list empty.
-- Terms and bindings from imported modules appear before terms and
-- bindings from the importing modules.
parseModuleResolveImports :: [FilePath] -- ^ List of files already
                                        -- loaded.  If a cycle is
                                        -- detected, fail.
                          -> FilePath
                          -> IO Module
parseModuleResolveImports fns fn = do
  when (fn `elem` fns) . fail $ "import cycle detected: " ^-^ fns
  m <- parseModule fn
  resolvedMs <- mapM (parseModuleResolveImports (fn:fns) . findModule fn)
                     (getModuleImports m)
  let m' = foldl (\mt m1 -> extendModule mt m1)
                 m resolvedMs
  return m' { getModuleImports = [] }
    where
      findModule :: FilePath -> String -> FilePath
      findModule = replaceBaseName

      extendModule mt m1 =
          mt { getModuleLets  = getModuleLets m1 ++ getModuleLets mt
             , getModuleCalls = getModuleCalls m1 ++ getModuleCalls mt
             , getModuleTypes = getModuleTypes m1 ++ getModuleTypes mt }
