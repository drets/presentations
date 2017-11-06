-- Code is written by kcsongor
-- Paper which should help to understand the code: “Hasochism”
-- (https://pdfs.semanticscholar.org/8b79/2f78825bad68cf7a2267ea03db5b4273df33.pdf)

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- Nats
data Nat = Z | S Nat

-- Singleton for n
data NatSing (n :: Nat) where
  ZSing :: NatSing 'Z
  SSing :: NatSing n -> NatSing ('S n)

data Fin (n :: Nat) where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)

-- Turn a runtime value into a nat with an upper bound
reifyFin :: forall n. Int -> NatSing n -> Maybe (Fin ('S n))
reifyFin 0 ZSing = Just FZ
reifyFin _ ZSing = Nothing
reifyFin 0 (SSing _) = Just FZ
reifyFin n (SSing m) = FS <$> reifyFin (n - 1) m

npred :: NatSing ('S n) -> NatSing n
npred (SSing n) = n

-- Vector

data Vect (n :: Nat) a where
  Nil  :: Vect 'Z a
  Cons :: a -> Vect n a -> Vect ('S n) a

get :: Fin n -> Vect n a -> a
get FZ (Cons x _) = x
get (FS n) (Cons _ xs) = get n xs

set :: Fin n -> a -> Vect n a -> Vect n a
set FZ x' (Cons _ xs) = Cons x' xs
set (FS n) x' (Cons x xs) = Cons x (set n x' xs)

vlength :: Vect n a -> NatSing n
vlength Nil = ZSing
vlength (Cons _ xs) = SSing (vlength xs)

-- Utils

instance Show a => Show (Vect n a) where
  show xs = "[" ++ show' xs ++ "]"
    where
      show' :: Vect m a -> String
      show' Nil = ""
      show' (Cons a b) = show a ++ ", " ++ show' b

-- Examples

test = Cons 1 (Cons 2 (Cons 3 Nil))
-- [1, 2, 3]
test' = get (FS FZ) test
-- 2

main = do
  print test
  putStrLn "Select an index"
  num <- readLn @Int
  case reifyFin num (npred (vlength test)) of
    Nothing -> putStrLn "out of bounds"
    Just l  -> print $ get l test
