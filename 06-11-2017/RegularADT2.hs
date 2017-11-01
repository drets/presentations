module RegularADT2 where


data Expr a
  = EInt Int
  | EAdd (Expr Int) (Expr Int)
  | EString String
  | EConcat (Expr String) (Expr String)

-- eval :: Expr a -> a
-- eval = undefined


-- • Couldn't match expected type ‘a’ with actual type ‘Int’
--   ‘a’ is a rigid type variable bound by
--     the type signature for:
--       eval :: forall a. Expr a -> a

-- eval :: Expr a -> a
-- eval (EInt i) = i


-- test = EInt 10 :: Expr String
-- “a” is just a phantom argument!
