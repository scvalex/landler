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
    show deriv = let tree = mkTree deriv
                     lvls = levels (design tree)
                     offset = 0 - findMin lvls
                     lins = layout offset lvls
                 in unlines $ reverse lins
        where
          mkTree :: Derivation -> Tree String
          mkTree d@(Ax _ _ _) = Tree (title d) []
          mkTree d@(ArrowI _ _ _ d1) = Tree (title d) [mkTree d1]
          mkTree d@(ArrowE _ _ _ d1 d2) = Tree (title d)
                                               [mkTree d1, mkTree d2]

          levels :: Tree (String, Int) -> [[(String, Int)]]
          levels (Tree (t, i) sts) =
              let lvls = map levels sts
                  lvls' = deepConcat lvls
              in [(t, i)] : lvls'

          findMin :: [[(String, Int)]] -> Int
          findMin = minimum . minimum . map (map (\(_, n) -> n))

          layout :: Int -> [[(String, Int)]] -> [String]
          layout offset = map (layoutLine offset "")

          layoutLine :: Int -> String -> [(String, Int)] -> String
          layoutLine _ acc [] = acc
          layoutLine offset acc ((t, i):tis) =
              let acc' = acc ++ replicate (offset + i - length acc) ' ' ++ t
              in layoutLine offset acc' tis

          deepConcat :: [[[a]]] -> [[a]]
          deepConcat = foldl concat2 []

          concat2 :: [[a]] -> [[a]] -> [[a]]
          concat2 [] yss = yss
          concat2 xss [] = xss
          concat2 (xs:xss) (ys:yss) = (xs ++ ys) : concat2 xss yss

          title (Ax cxt term typ) =
              "(Ax) " ^-^ (showCxt cxt) ^-^
              " ⊢  " ^-^ term ^-^ " : " ^-^ typ
          title (ArrowI cxt term typ _) =
              "(→ I) " ^-^ (showCxt cxt) ^-^
              " ⊢ " ^-^ term ^-^ " : " ^-^ typ
          title (ArrowE cxt term typ _ _) =
              "(→ E) " ^-^ (showCxt cxt) ^-^
              " ⊢ " ^-^ term ^-^ " : " ^-^ typ

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

----------------------------------------------------------------------
-- Tree layout helpers
--
-- Thank you: Andrew J. Kennedy
--            Functional Pearls: Drawing Trees
--            J. Functional Programming 6 (3): 527-534
--            Cambridge University Press, May 1996
----------------------------------------------------------------------

data Tree a = Tree a [Tree a]
              deriving ( Show )

type Extent = [(Int, Int)]

moveTree :: Tree (a, Int) -> Int -> Tree (a, Int)
moveTree (Tree (cargo, pos) subtrees) disp = Tree (cargo, pos + disp) subtrees

moveExtent :: Extent -> Int -> Extent
moveExtent e disp = map (\(x, y) -> (x + disp, y + disp)) e

merge :: Extent -> Extent -> Extent
merge [] qs = qs
merge ps [] = ps
merge ((p, _) : ps) ((_, q) : qs) = (p, q) : merge ps qs

mergeList :: [Extent] -> Extent
mergeList = foldr merge []

fit :: Extent -> Extent -> Int
fit ((_, p) : ps) ((q, _) : qs) = max (fit ps qs) (p - q + 4)
fit _             _             = 0

fitList :: [Extent] -> [Int]
fitList exts = zipWith mean (fitListL exts) (fitListR exts)
    where
      mean x y = (x + y + 1) `div` 2

      fitListL :: [Extent] -> [Int]
      fitListL = go []
          where
            go _ []       = []
            go acc (e:es) = let x = fit acc e
                            in x : go (merge acc (moveExtent e x)) es

      fitListR :: [Extent] -> [Int]
      fitListR = reverse . go [] . reverse
          where
            go _ []       = []
            go acc (e:es) = let x = 0 - (fit e acc)
                            in x : go (merge (moveExtent e x) acc) es

design :: Tree String -> Tree (String, Int)
design = fst . go
    where
      go (Tree cargo subtrees) =
          let (trees, extents) = unzip $ map go subtrees
              positions = fitList extents
              ptrees = zipWith moveTree trees positions
              pextents = zipWith moveExtent extents positions
              resultExtent = (0, length cargo) : mergeList pextents
              resultTree = Tree (cargo, 0) ptrees
          in (resultTree, resultExtent)
