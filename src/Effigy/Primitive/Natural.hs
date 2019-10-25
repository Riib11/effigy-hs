module Effigy.Primitive.Natural where

{-
  ℕ
-}


data ℕ = Zero | Successor ℕ

pattern Z = Zero
pattern S n = Successor n


{-
  Semigroup instance
-}


instance Semigroup ℕ where
  Z <> n = n
  S m <> n = S (m <> n)


{-
  Show instance
-}


instance Show ℕ where
  show = \case
    Zero        -> "Z"
    Successor n -> "S" ++ show n
