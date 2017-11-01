{-# LANGUAGE GADTs #-}
module GADTS1 where

-- allow you to explicitly write down the types of the constructors

data Expr a where
  EInt :: Int -> Expr Int
  EAdd :: Expr Int -> Expr Int -> Expr Int
  EString :: String -> Expr String
  EConcat :: Expr String -> Expr String -> Expr String
  -- EPair :: Expr a -> Expr b -> Expr (a, b)

eval :: Expr a -> a
eval (EInt i) = i
eval (EAdd e1 e2) = eval e1 + eval e2
eval (EString s) = s
eval (EConcat e1 e2) = eval e1 ++ eval e2

test1 = eval (EAdd (EInt 5) (EInt 6))
-- test2 = eval (EAdd (EString "this is") (EString " wrong"))
test3 = eval (EConcat (EString "this is") (EString " correct"))
