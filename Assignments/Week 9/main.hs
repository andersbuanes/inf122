import Data.Char ( isDigit, digitToInt )

-- Oppgave A

gRep :: (t -> Bool) -> t -> [t] -> [t]
gRep pr y xs = helper pr y xs []
        where helper pr y xs acc
                | null (tail xs) = if pr (head xs) then [y] else xs
                | pr (head xs) = acc ++ [y] ++ helper pr y (tail xs) acc
                | otherwise = acc ++ [head xs] ++ helper pr y (tail xs) acc

gRep' :: (t -> Bool) -> t -> [t] -> [t]
gRep' _ _ [] = []
gRep' pr y xs = if pr (head xs) 
                    then y : gRep' pr y (tail xs)
                else head xs : gRep' pr y (tail xs)

gRepM :: (t -> Bool) -> t -> [t] -> [t]
gRepM pr y = map (\x -> if pr x then y else x)


-- Oppgave D
next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (==0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1..] board]
        where update r n = if r == row then n - num else n

putRow :: Int -> Int -> IO()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO()
putBoard [a, b, c, d, e] = do putRow 1 a
                              putRow 2 b
                              putRow 3 c
                              putRow 4 d
                              putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do putStrLn "ERROR: Invalid digit"
                           getDigit prompt

newline :: IO()
newline = putChar '\n'

undo :: [Board] -> Board
undo xs = if null (tail xs) || null xs then initial else last xs

play :: Board -> Int -> [Board] -> IO()
play board player moves =
    do newline
       putBoard board
       if finished board then
           do newline
              putStr "Player "
              putStr (show (next player))
              putStrLn " wins!!"
        else
            do newline
               putStr "Player "
               print player
               row <- getDigit "Enter a row number, or 0 to undo move: "
               if row == 0 then
                   play (undo moves) player (init moves)
               else do
                   num <- getDigit "Stars to remove : "
                   if valid board row num then
                       play (move board row num) (next player) (moves ++ [board])
                   else
                       do newline
                          putStrLn "ERROR: Invalid move"
                          play board player moves

nim :: IO()
nim = play initial 1 []