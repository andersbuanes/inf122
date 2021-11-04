-- Eksempel quicksort

qs :: [Int] -> [Int]
qs (x:xs) = qs[y|y <- xs, y<x] ++ [x] ++ qs[y|y <- xs, y>=x]

rq :: [Int] -> [Int]
rq (x:xs) = rq [y|y <- xs, y>=x] ++[x]++ rq[y|y <- xs, y<x]

mind :: Int -> [Int] -> [Int]
mind x [] = []
--mind x (y:ys) = if (x > y) then (y:mind x ys) else (mind x ys)
mind x ys = [y | y <- ys, y<x]

str :: Int -> [Int] -> [Int]
str x [] = []
str x (y:ys) = if (x <= y) then y:str x ys else str x ys
