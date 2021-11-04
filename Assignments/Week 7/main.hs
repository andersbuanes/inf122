import Prelude hiding (pi)
data Ast = V Int | P Ast Ast | M Ast Ast

eval :: Ast -> Int
eval (V x) = x
eval (P left right) = eval left + eval right
eval (M left right) = eval left * eval right

evalb :: Ast -> Bool
evalb (V x) = odd x
evalb (P left right) = evalb left || evalb right
evalb (M left right) = evalb left && evalb right

-- Keeping previous tasks to avoid errors

-- Oppgave C
ev ::  Ast -> (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> a
ev (V x) v _ _ = v x
ev (P left right) v p m = p (ev left v p m) (ev right v p m)
ev (M left right) v p m = m (ev left v p m) (ev right v p m)

vi :: Int -> Int
vi x = x
pi :: (Int -> Int -> Int)
pi x y = x + y
mi :: (Int -> Int -> Int)
mi x y = x * y

vb :: Int -> Bool
vb x = odd x
pb :: Bool -> Bool -> Bool
pb x y = x || y
mb :: Bool -> Bool -> Bool
mb x y = x && y

vStr :: Show a => a -> String
vStr x = show x
pStr :: (String -> String -> String)
pStr x y = "(" ++ x ++ " + " ++ y ++ ")"
mStr :: (String -> String -> String)
mStr x y = "(" ++ x ++ " * " ++ y ++ ")"


-- Oppgave D
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val a) = f a
folde f g (Add a b) = g (folde f g a) (folde f g b)