{-# LANGUAGE FlexibleInstances #-}

module Named where

import           Control.Applicative hiding ((<|>))
import           Control.Arrow       (second)
import           Data.List           (find)
import           Data.Maybe          (fromJust)
import           Text.Parsec
import           Text.Parsec.String

data Term a = Abst a (Term a)
            | App (Term a) (Term a)
            | Var a
            deriving Eq

instance Show (Term String) where
   show (Var a) = a
   show (App e1 e2@(App _ _)) = concat [show e1, " (", show e2, ")"]
   show (App e1 e2) = show e1 ++ " " ++ show e2
   show (Abst b e)  = concat ["(\\", b, ".", show e, ")"]

free :: Eq a => Term a -> [a]
free = free' []
  where free' bound (Var a) = if a `elem` bound
                              then []
                              else [a]
        free' bound (App e1 e2) = free' bound e1 ++ free' bound e2
        free' bound (Abst v e) = free' (v:bound) e

sub :: (Eq a, Freshable a) => Term a -> a -> Term a -> Term a
sub (Var a) find replace = if a == find
                           then replace
                           else Var a
sub (App e1 e2) find replace = App (sub e1 find replace) (sub e2 find replace)
sub (Abst b e) find replace  | find == b                = Abst b e
                             | b `notElem` free replace = Abst b (sub e find replace)
                             | otherwise                = let freshName = fresh (free replace ++ free e)
                                                          in Abst freshName (sub (sub e b (Var freshName)) find replace)

reduce :: (Eq a, Freshable a) => Term a -> Term a
reduce (Abst b (App f (Var a))) | a == b = f
reduce (App (Abst b e1) e2) = sub e1 b e2
reduce (App e1 e2) = App (reduce e1) (reduce e2)
reduce (Abst b e) = Abst b (reduce e)
reduce e = e

reducefull :: (Eq a, Freshable a) => Term a -> Term a
reducefull = fst . fromJust . find (uncurry (==)) . pairify . iterate reduce

pairify :: [a] -> [(a,a)]
pairify (x:y:zs) = (x,y):pairify zs

class Freshable a where
  fresh :: [a] -> a

instance Freshable Char where
  fresh cs = case find (`notElem` cs) (['a'..'z']++['A'..'Z']) of
    Just c -> c
    Nothing -> error "characters are not infinitely many :("


combos :: [a] -> [[a]]
combos xs = let xs' = map (:[]) xs
                combo' ys = ys ++ [x:y | x <- xs, y <- ys]
            in combo' xs'

instance Freshable String where
  fresh lists = case find (`notElem` lists) . combos $ ['a'..'z'] ++ ['A'..'Z'] of
    Just str -> str
    Nothing -> error "Couldn't find a fresh string. This should not happen"


parseTermression :: Parser (Term String)
parseTermression = (Abst <$> (spaces *> char '\\' *> spaces *> many1 letter <* spaces <* char '.' <* spaces) <*> parseTermression) <|>
                  (foldl1 App <$> term `sepBy` many1 space)
  where term = (Var <$> many1 letter) <|> (char '(' *> spaces *> parseTermression <* spaces <* char ')')

readTerm :: String -> Maybe (Term String)
readTerm str = case parse parseTermression "" str of
  Left _  -> Nothing
  Right e -> Just e

omega = fromJust . readTerm $ "(\\x.x x) (\\x.x x)"
omega2 = fromJust . readTerm $ "(\\x.x x x) (\\x.x x x)"







bind env exp = foldr (\(name,bind) exp -> App (Abst name exp) bind) exp env

lets = map (second expr)
  [("id", "\\x.x"),
   ("const", "\\a.\\b.a"),
   ("omega", "(\\x.x x) (\\x.x x)"),
   ("z", "\\f.\\x.x"),
   ("s", "\\n.\\f.\\x. f (n f x)"),
   ("plus", "\\n.\\m. n s m"),
   ("pred","\\n.\\f.\\x.n (\\g.\\h.h (g f)) (const x) id"),
   ("minus", "\\n.\\m. m pred n"),
   ("times", "\\n.\\m.\\f. n (m f)"),
   ("pow", "\\x.\\y.(times y (times x)) (s z)"),
   ("true", "\\t.\\f.t"),
   ("false", "\\t.\\f.f"),
   ("not", "\\b.\\t.\\f.b f t"),
   ("or", "\\p.\\q.p p q"),
   ("and", "\\p.\\q.p q p"),
   ("if", "\\b.\\t.\\f. b t f"),
   ("isZero", "\\n.n (const false) true"),
   ("fix", "\\f. (\\x.f (x x)) (\\x.f (x x))"),
   ("fact", "fix (\\fct.\\n. (isZero n) (s z) (times n (fct (pred n))))")
   ]

expr = fromJust . readTerm
