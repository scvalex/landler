{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative
import Language.Landler
import Test.QuickCheck

instance Arbitrary Term where
  -- Maximum depth: 7
  arbitrary = choose (0, 7) >>= go
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

prop_IdemParseShow :: Term -> Bool
prop_IdemParseShow x = x == toTerm (show x)
