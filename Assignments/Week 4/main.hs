-- Oppgave C
s :: (t1 -> t2 -> t3) -> (t1 -> t2) -> t1 -> t3
s = \ f g x -> f x (g x)

k :: p1 -> p2 -> p1
k = \ x y -> x


-- s k k n
-- (\ f g x -> f x (g x)) k k n
-- (\ g x -> k x (g x)) k n
-- (\ x -> k x (k x)) n
-- k n (k n)
-- (\ x y -> x) n (k n)
-- (\ y -> n) (k n)
-- n

-- Dette viser at s :: t1 -> t1

-- Oppgave F
rem1 :: Eq a => [a] -> a -> [a]
rem1 [] _ = []
rem1 (x:xs) y | x == y    = xs
              | otherwise = x : rem1 xs y