module DeBruijn where

import           Data.Function (on)

-- Natural Numbers --
data Nat = Z | S Nat
         deriving (Eq, Ord)

toInt :: Nat -> Integer
toInt  Z    = 0
toInt (S n) = 1 + toInt n

fromInt :: Integer -> Nat
fromInt x | x < 0     = error "Cannot convert a negative to a Nat"
          | x == 0    = Z
          | otherwise = S (fromInt (x - 1))

instance Num Nat where
  n + Z        = n
  Z + n        = n
  (S n) + m    = n + S m

  n - Z        = n
  Z - _        = error "No negatives in Naturals"
  (S n) - (S m)    = n - m

  x * y        = fromInt (toInt x * toInt y)

  abs          = id

  signum Z     = 0
  signum (S _) = 1

  fromInteger  = fromInt

instance Show Nat where
  show = show . toInt


-- Lambda Terms --
data Term = Index Nat
          | App Term Term
          | Abst Term
          deriving Eq

instance Show Term where
  show (Index n)                      = show n
  show (Abst t)                       = "λ." ++ show t
  show (App t1@(Abst _) t2@(App _ _)) = concat ["(", show t1, ") (", show t2, ")"]
  show (App t1@(Abst _) t2)           = "(" ++ show t1 ++ ") " ++ show t2
  show (App t1 t2@(Abst _))           = show t1 ++ " (" ++ show t2 ++ ")"
  show (App t1 t2@(App _ _))          = show t1 ++ " (" ++ show t2 ++ ")"
  show (App t1 t2)                    = show t1 ++ " " ++ show t2


shift :: Integer -> Integer -> Term -> Term
shift amount cutoff (Index n)   | toInt n < cutoff  = Index n
                                | toInt n >= cutoff = Index . fromInt $ toInt n + fromInteger amount
shift amount cutoff (App t1 t2) = App (shift amount cutoff t1) (shift amount cutoff t2)
shift amount cutoff (Abst t)    = Abst (shift amount (cutoff + 1) t)

sub, (-->) :: Nat -> Term -> Term -> Term
(find --> replace) (Index n)   | find == n = replace
                               | otherwise = Index n
(find --> replace) (App t1 t2) = App ((find --> replace) t1) ((find --> replace) t2)
(find --> replace) (Abst t)    = Abst ((S find --> shift 1 0 replace) t)

sub = (-->)

reduce :: Term -> Term
reduce (App (Abst t1) t2)       = shift (-1) 0 ((0 --> shift 1 0 t2) t1)
reduce (App t1 t2)              = App (reduce t1) (reduce t2)
reduce (Abst (App f (Index 0))) = f
reduce t                        = t

-- Convenience --

($-) :: Term -> Term -> Term
($-) = App

app :: [Term] -> Term
app = foldl1 App

i0, i1, i2, i3 :: Term
i0 = Index 0
i1 = Index 1
i2 = Index 2
i3 = Index 3

z :: Term
z = Abst (Abst i0)

s :: Term
s = Abst (Abst (Abst (App i1 (App (App i2  i1) i0))))

plus :: Term
plus = Abst (Abst (Abst (Abst (App (App i3 i1) (App (App i2 i0) i1)))))


