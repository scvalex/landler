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

      ab :: Int -> Gen Term
      ab n = Ab <$> name <*> (go (n - 1))

      app :: Int -> Gen Term
      app n = App <$> (go (n - 1)) <*> (go (n - 1))

instance Arbitrary Statement where
    arbitrary = choose (0, 1) >>= go
        where
          go :: Int -> Gen Statement
          go 0 = Let <$> name <*> arbitrary
          go _ = Call <$> arbitrary

main :: IO ()
main = do
  quickCheck prop_IdemParseShow
  quickCheck prop_IdemParseShowStatement

prop_IdemParseShow :: Term -> Property
prop_IdemParseShow x = not (isVar x) ==> (Just x) == toTerm (show x)
    where
      isVar :: Term -> Bool
      isVar (Var _) = True
      isVar _       = False

prop_IdemParseShowStatement :: Statement -> Bool
prop_IdemParseShowStatement x = (Right x) == parseStatement (show x)

name :: Gen String
name = choose (0, 100) >>= \i -> return (allVars !! i)
    where
      allVars = let vs = "" : [v ++ [s] | v <- vs, s <- ['a'..'z']]
                in tail vs
