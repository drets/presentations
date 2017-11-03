{-# LANGUAGE GADTs #-}

module GADTs where

data Expr a where
  ELit    :: a -> Expr a
  EFun    :: (a -> b) -> Expr (a -> b)
  EApp    :: Expr (a -> b) -> Expr a -> Expr b

eval :: Expr a -> a
eval (ELit l) = l
eval (EFun f) = f
eval (EApp f a) = eval f $ eval a

add = EApp (EApp (EFun (+)) (ELit 10)) (ELit 10)
