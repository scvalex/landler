{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative
import Language.Landler
import Test.QuickCheck

instance Arbitrary Term where
  arbitrary = choose (1, 9) >>= go
    where
      go :: Int -> Gen Term
      go 0 = Var <$> name
      go n = oneof [Var <$> name, ab n, app n]

      name :: Gen String
      name = choose (0, 100) >>= \i -> return (allVars !! i)
        where
          allVars = let vs = "" : [v ++ [s] | v <- vs, s <- ['a'..'z']]
                    in tail vs

      ab :: Int -> Gen Term
      ab n = Ab <$> name <*> (go (n - 1))

      app :: Int -> Gen Term
      app n = App <$> (go (n - 1)) <*> (go (n - 1))

main :: IO ()
main = mapM_ quickCheck [prop_IdemParseShow]

prop_IdemParseShow :: Term -> Property
prop_IdemParseShow x = not (isVar x) ==> x == toTerm (show x)
    where
      isVar :: Term -> Bool
      isVar (Var _) = True
      isVar _       = False
