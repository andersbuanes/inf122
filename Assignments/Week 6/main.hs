-- Oppgave A

remg :: [a] -> (a -> Bool) -> [a]
remg [] f = []
remg xs f = remgHelper xs f []
    where remgHelper (x:xs) f acc
            | null xs = acc ++ [x]
            | f x = acc ++ xs
            | otherwise = remgHelper xs f (acc ++ [x])


-- Oppgave 7.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g xs = altMapHelper f g xs 0
    where altMapHelper f g xs n
            | null xs = []
            | even n = f (head xs) : altMapHelper f g (tail xs) (n + 1)
            | otherwise = g (head xs) : altMapHelper f g (tail xs) (n + 1)