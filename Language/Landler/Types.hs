{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances #-}

module Language.Landler.Types (
        -- * Statements and terms
        Environment, Statement(..), Var, Term(..), Module(..),

        -- * Evalutaion results
        Result(..), Step,

        -- * Types and derivations
        Derivation(..), Context, Type(..),

        -- * Errors and exceptions
        Error(..),

        -- * Helper functions
        allVars, canonicalForm, getDerivationType, showCxt
    ) where

import qualified Control.Exception as CE
import Data.List ( intercalate )
import qualified Data.Map as M
import Data.Typeable ( Typeable )
import Text.Interpol ( (^-^) )

----------------------------------------------------------------------
-- Statements and terms
----------------------------------------------------------------------

-- | An 'Environment' maps names to the terms they represent.
type Environment = [(Var, Term)]

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
               | DeriveS Term
                 deriving ( Eq )

instance Show Statement where
    show (LetS v t)   = "let " ^-^ v ^-^ " = (" ^-^ t ^-^ ")"
    show (CallS t)    = "(" ^-^ t ^-^ ")"
    show (ImportS mn) = "import " ^-^ mn
    show (TypeS t)    = "type " ^-^ t
    show (DeriveS t)  = "derive " ^-^ t

-- | Lambda-calculus variable names follow the same rules as Haskell
-- identifiers.  Basically, the first character must be a letter or
-- @_@ and subesequent characters may be letters, numbers and @_@.
type Var = String

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
                     , getModuleDerives :: [Term]
                     } deriving ( Show )

----------------------------------------------------------------------
-- Evaluation results
----------------------------------------------------------------------

-- | The result of executing a 'Statement'.
data Result = CallR [Step]
            | TypeR Type
            | DeriveR Derivation

instance Show Result where
    show (CallR steps) = unlines $ go steps
        where
          go []            = ["---"]
          go ((t, s) : ts) = (show t) : ("\t" ^-^ s) : go ts
    show (TypeR typ) = show typ
    show (DeriveR derivation) = show derivation

-- | A 'Step' is a term and a description of the reduction (if any)
-- that can be applied to it.
type Step = (Term, String)

----------------------------------------------------------------------
-- Types and derivations
----------------------------------------------------------------------

type Context = M.Map Var Type

data Derivation = Ax Context Term Type
                | ArrowI Context Term Type Derivation
                | ArrowE Context Term Type Derivation Derivation

instance Show Derivation where
    show (Ax cxt term typ) = "(Ax) " ^-^ (showCxt cxt) ^-^
                             " ⊢ " ^-^ term ^-^ " : " ^-^ typ
    show (ArrowI cxt term typ deriv) = "(→ I) " ^-^ (showCxt cxt) ^-^
                                       " ⊢ " ^-^ term ^-^ " : " ^-^ typ ^-^
                                       "\n" ^-^ deriv
    show (ArrowE cxt term typ deriv1 deriv2) =
        "(→ E) " ^-^ (showCxt cxt) ^-^
        " ⊢ " ^-^ term ^-^ " : " ^-^ typ ^-^
        "\n" ^-^ deriv1 ^-^ "\n" ^-^ deriv2

data Type = TypeVar Var
          | TypeArr Type Type
            deriving ( Eq )

instance Show Type where
    show = go . canonicalForm
        where
          go (TypeVar v) = v
          go (TypeArr (TypeVar v) t) = v ^-^ " → " ^-^ go t
          go (TypeArr t1 t2) = "(" ^-^ go t1 ^-^ ") → " ^-^ go t2

----------------------------------------------------------------------
-- Errors and exceptions
----------------------------------------------------------------------

-- | The kinds of errors landler throws.
data Error = TypeError String
           | ParseError Int         -- ^ Line number
                        Int         -- ^ Column number
                        String      -- ^ Insightful message
             deriving ( Typeable )

instance CE.Exception Error

instance Show Error where
    show (TypeError msg) = "Type error: " ^-^ msg
    show (ParseError line col msg) =
        let msgs = filter (not . null) $ lines msg
            msg' = unlines $ map ("    " ^-^) msgs
        in "" ^-^ line ^-^ ":" ^-^ col ^-^ ":\n" ^-^ msg'

----------------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------------

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

-- | Get the final type of a derivation (i.e. the type in the root of
-- the tree).
getDerivationType :: Derivation -> Type
getDerivationType (Ax     _ _ t)     = t
getDerivationType (ArrowI _ _ t _)   = t
getDerivationType (ArrowE _ _ t _ _) = t

showCxt :: Context -> String
showCxt = intercalate ", " .
          M.foldrWithKey (\v t acc -> (v ^-^ " : " ^-^ t) : acc) []
