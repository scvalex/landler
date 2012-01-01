{-# LANGUAGE DeriveDataTypeable #-}

module Language.Landler.Types (
        Module(..), Statement(..), Term(..), Var,

        Environment, Result(..), Step, Type(..), Error(..),

        allVars, canonicalForm
    ) where

import qualified Control.Exception as CE
import qualified Data.Map as M
import Data.Typeable ( Typeable )
import Text.Interpol ( (^-^) )

-- | Lambda-calculus variable names follow the same rules as Haskell
-- identifiers.  Basically, the first character must be a letter or
-- @_@ and subesequent characters may be letters, numbers and @_@.
type Var = String

-- | Statements are one of:
--
--   [@let <var> = (<term>)@] binds the term to the variable name;
--
--   [@(<term>)@] evaluates the term and prints out the result;
--
--   [@import <module>@] brings all lets in the given module into
--   scope and evaluates any free terms in order; or
--
--   [@type (<term>)@] prints the derivied type for the term.
--
--  These are meta-syntactic constructs; they are not part of the
--  lambda-calculus.
data Statement = LetS Var Term | CallS Term | ImportS String | TypeS Term
                 deriving ( Eq )

instance Show Statement where
    show (LetS v t)   = "let " ^-^ v ^-^ " = (" ^-^ t ^-^ ")"
    show (CallS t)    = "(" ^-^ t ^-^ ")"
    show (ImportS mn) = "import " ^-^ mn
    show (TypeS t)    = "type " ^-^ t

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

-- | A 'Module' encapsulates the lambda-data found in a file.
data Module = Module { getModuleName :: String
                     , getModulePath :: FilePath
                     , getModuleImports :: [String]
                     , getModuleLets :: [(Var, Term)]
                     , getModuleCalls :: [Term]
                     , getModuleTypes :: [Term]
                     } deriving ( Show )

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
            deriving ( Eq )

instance Show Type where
    show = go . canonicalForm
        where
          go (TypeVar v) = v
          go (TypeArr (TypeVar v) t) = v ^-^ " → " ^-^ go t
          go (TypeArr t1 t2) = "(" ^-^ go t1 ^-^ ") → " ^-^ go t2

-- | The kinds of errors landler throws.  Also have a look at
-- 'ParseError'.
data Error = TypeError String
             deriving ( Show, Typeable )

instance CE.Exception Error

-- | Rename the 'TypeVar's in a 'Type' to a more *humane* order.  For
-- instance, turn @B -> C -> A@ into @A -> B -> C@.
canonicalForm :: Type -> Type
canonicalForm t = let (cf, _, _) = go allVars M.empty t
                  in cf
    where
      go (v:vs) rcxt (TypeVar v1) =
          case M.lookup v1 rcxt of
            Nothing -> (TypeVar v, M.insert v1 v rcxt, vs)
            Just v2 -> (TypeVar v2, rcxt, v:vs)
      go vs rcxt (TypeArr t1 t2) =
          let (t1', rcxt', vs') = go vs rcxt t1
              (t2', rcxt'', vs'') = go vs' rcxt' t2
          in (TypeArr t1' t2', rcxt'', vs'')
      go _ _ _ = error "cannot happen"

-- | A lot of variable names.
allVars :: [Var]
allVars = let vs = "" : [v ++ [s] | v <- vs, s <- ['a'..'z']]
          in tail vs
