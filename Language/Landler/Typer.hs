module Language.Landler.Typer (
        principalType, principalType'
    ) where

import qualified Control.Exception as CE
import Control.Monad.State ( MonadState(..), State, evalState )
import qualified Data.Map as M
import Data.Maybe ( fromJust )
import Language.Landler.Parser ( ReadTerm(..) )
import Language.Landler.Types ( Var, allVars
                              , Environment, Type(..), Term(..)
                              , Context, Derivation(..), getDerivationType
                              , Error(..) )
import Text.Interpol ( (^-^) )

----------------------------------------------------------------------
-- Types
----------------------------------------------------------------------

-- | A 'Substitution' is a function that transforms one type into
-- another.
type Substitution = Type -> Type

----------------------------------------------------------------------
-- Principal-type algorirthm (alternative version)
----------------------------------------------------------------------

-- | Determine the type for the given term, taking into account the
-- given environment.
principalType :: (ReadTerm t) => Environment -> t -> Type
principalType env = getDerivationType . principalType' env

-- | Determine the derivation to type the given term, taking into
-- account the givent environment.
principalType' :: (ReadTerm t) => Environment -> t -> Derivation
principalType' _ rt = let t = toTerm rt
                     in snd $ runFresh (mkContext t M.empty >>= go t)
    where
      mkContext :: Term -> Context -> Fresh Context
      mkContext (Var v) cxt = case M.lookup v cxt of
                                    Nothing -> do
                                      t <- fresh
                                      return $ M.insert v (TypeVar t) cxt
                                    Just _  -> return cxt
      mkContext (App t1 t2) cxt = mkContext t1 cxt >>= mkContext t2
      mkContext (Ab _ t) cxt = mkContext t cxt

      go :: Term -> Context -> Fresh (Substitution, Derivation)
      go term@(Var v) cxt =
          return (id, Ax cxt term (fromJust $ M.lookup v cxt))
      go term@(Ab v t) cxt  = do
              t1 <- fresh
              (s, d2) <- go t (M.insert v (TypeVar t1) cxt)
              return (s, ArrowI cxt term (s (TypeArr (TypeVar t1)
                                                     (getDerivationType d2)))
                                d2)
      go term@(App m n) cxt = do
              t1 <- fresh
              (s3, d3) <- go m cxt
              (s2, d2) <- go n (contextSubstitution s3 cxt)
              let s1 = unifyTypes (s2 (getDerivationType d3))
                                  (TypeArr (getDerivationType d2)
                                           (TypeVar t1))
              return (s1 . s2 . s3, ArrowE cxt term (s1 (TypeVar t1)) d2 d3)

      -- | Robinson's unification algorithm:
      unifyTypes :: Type -> Type -> Substitution
      unifyTypes t1@(TypeVar v1) t2
            | t1 == t2  = id
            | otherwise = occursCheck t1 t2 $ replaceType v1 t2
      unifyTypes t1 t2@(TypeVar _) = unifyTypes t2 t1
      unifyTypes (TypeArr a b) (TypeArr c d) =
          let s1 = unifyTypes a c
              s2 = unifyTypes (s1 b) (s1 d)
          in s2 . s1

      contextSubstitution :: Substitution -> Context -> Context
      contextSubstitution s = M.map s

      occursCheck :: Type -> Type -> a -> a
      occursCheck t1 (TypeArr t2 t3) x = occursCheck t1 t2 $
                                         occursCheck t1 t3 $ x
      occursCheck t1 t2 x
            | t1 == t2  = CE.throw (occursCheckFailed t1 t2)
            | otherwise = x


      replaceType :: Var -> Type -> Type -> Type
      replaceType v1 t2 t3@(TypeVar v) = if v == v1 then t2 else t3
      replaceType v1 t2 (TypeArr t3 t4) = TypeArr (replaceType v1 t2 t3)
                                                  (replaceType v1 t2 t4)

      occursCheckFailed t1 t2 = TypeError $ "Occurs check failed:\n\t" ^-^
                                            t1 ^-^ "\nin\n\t" ^-^ t2

----------------------------------------------------------------------
-- The Fresh monad
----------------------------------------------------------------------

-- | A monad for fresh variables.  See 'fresh'.
type Fresh = State [Var]

-- | Get a fresh variable name.  This variable name is guaranteed to
-- be new.
fresh :: Fresh Var
fresh = do
  (v:vs) <- get
  put vs
  return v

-- | Run the 'Fresh' monad.
runFresh :: Fresh a -> a
runFresh f = evalState f allVars
