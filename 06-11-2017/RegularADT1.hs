module RegularADT where


data Expr
  = EInt Int
  | EAdd Expr Expr
  | EString String
  | EConcat Expr Expr

eval :: Expr -> Int
eval (EInt i) = i
eval (EAdd e1 e2) = eval e1 + eval e2
eval (EString s) = error "not an Int"
eval (EConcat _ _) = error "not an Int"

test = eval (EAdd (EInt 5) (EInt 6))
