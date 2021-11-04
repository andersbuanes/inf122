sp :: Int -> [a] -> ([a],[a])
sp n xs = if n <= 0 then ([], xs)
               else if (n >= length xs) then (xs, [])
               else let (a, b) = sp (n-1) (tail xs) in (head xs:a,b)

-- betingende likninger
og :: Bool -> Bool -> Bool
og x y = if x == False then False 
         else if y == False then False 
         else True
     
-- voktede likninger
og_guard :: Bool -> Bool -> Bool
og_guard x y | x == False = x 
             | y == False = y
             | otherwise = True

