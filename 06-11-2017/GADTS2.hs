{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module GADTS2 where

import GHC.TypeLits

-- data Expr :: Nat -> * -> * where
-- data Expr (l :: Nat) (a :: *) where
data Expr l a where
  EInt :: Int -> Expr 1 Int
  EAdd :: Expr a Int -> Expr b Int -> Expr (a + b) Int
  EString :: String -> Expr 1 String
  EConcat :: Expr a String -> Expr b String -> Expr (a + b) String

-- only evaluate expressions of size 10
-- the recursive calls are going to be on smaller expressions,
-- so this is not very useful.
eval :: Expr 10 a -> a
eval = undefined
