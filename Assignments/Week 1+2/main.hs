-- C
dobb :: [Int] -> [Int]
dobb [] = []
dobb [x] = [x*2]
dobb (x:xs) = x*2 : dobb xs

-- D
fire :: [Int] -> [Int]
fire [] = []
fire [x] = [x*4]
fire (x:xs) = x*4 : fire xs

-- E
flett :: [Int] -> [Int] -> [Int]
flett [] xs = xs
flett ys [] = ys
flett (x:xs) (y:ys) = if x <= y  
        then x:flett xs (y:ys)
        else y:flett (x:xs) ys

-- F
ele :: Int -> [a] -> a
ele 1 (x:xs) = x
ele a (x:xs) = ele (a-1) xs

-- G
addobb :: [Int] -> [Int]
addobb [] = []
addobb xs = xs ++ dobb xs 

-- H
pali :: (Eq a) => [a] -> Bool
pali xs = xs == reverse xs
