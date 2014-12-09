module TypedBoolean where

import           Control.Monad (liftM)
import           Data.Foldable
import           Prelude       hiding (concat)

data Term = Var Symbol
          | App Term Term
          | Lambda Symbol Type Term
          | Prim Primitive

data Primitive = BoolPrim Bool
               | NandPrim
               | ZeroPrim
               | SuccPrim

data Type = Type :-> Type
          | BoolType
          | NatType
            deriving Eq

type TypingContext = [(Symbol, Type)]
type Symbol = String


check :: TypingContext -> Term -> Either String Type
check _ (Prim (BoolPrim _)) = Right BoolType
check _ (Prim NandPrim)     = Right (BoolType :-> (BoolType :-> BoolType))
check _ (Prim ZeroPrim)     = Right NatType
check _ (Prim SuccPrim)     = Right (NatType :-> NatType)

check context (Var x)     = case lookup x context of
  Just itsType -> Right itsType
  Nothing      -> Left ("Type of " ++ x ++ " not in scope")

check context (Lambda x argType term) = liftM (argType :->) (check ((x,argType):context) term)

check context (App t1 t2) = do
  (t1Domain :-> t1Codomain) <- check context t1
  t2Type                    <- check context t2
  if t2Type == t1Domain
    then Right t1Codomain
    else Left (concat ["Expected an argument of type ", show t1Domain, ", but got a ", show t2Type, "."])

nand :: Bool -> Bool -> Bool
nand b1 b2 = not (b1 && b2)

instance Show Type where
  show BoolType = "Bool"
  show (t1@(_ :-> _) :-> t2) = concat ["(", show t1, ") -> ", show t2]
  show (t1 :-> t2) = show t1 ++ " -> " ++ show t2

instance Show Primitive where
  show (BoolPrim b) = show b
  show NandPrim     = "⊼"

instance Show Term where
  show (Var s)  = s
  show (Prim p) = show p
  show (Lambda x argType term)  = concat ["λ", x, ":", show argType, ".", show term]
  show (App l@(Lambda _ _ _) t) = concat ["(", show l, ") ", show t]
  show (App t l@(Lambda _ _ _)) = concat [show t, " (", show l, ")"]
  show (App t1 t2@(App _ _))    = concat [show t1, " (", show t2, ")"]
  show (App t1 t2)              = show t1 ++ " " ++ show t2
