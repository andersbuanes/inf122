module Oblig2 where

import Control.Concurrent ( threadDelay )
import System.Exit ( exitSuccess )
import Text.Read ( readMaybe )

type Tower = [Int]    -- 
type State = [Tower]  -- Current state of the towers
type Pos = (Int, Int) -- Positional coordinates in the terminal

main :: IO()
main = do
    clr
    putStrLn "Start a new game with: b <nbOfRings>, or quit with: q"
    cmd <- getCmd "> "
    case words cmd of
        ["b", n] -> do isDig <- isDigit n
                       if isDig
                           then begin (read n)
                           else main

        ["q"   ] -> do putStrLn "Stopping the program."
                       return()

        ["help"] -> do help
                       delay 6
                       main

        _        -> do errCmd
                       main

-- | Suspends the current thread for n seconds
delay :: Int -> IO()
delay n = threadDelay (n * 1000000)

minimumSteps :: Int -> Int
minimumSteps n = 2^n - 1

-- | Generates a new game state of size
generate :: Int -> State
generate n = [[1..n], [], []]

begin :: Int -> IO ()
begin n = do
    let upperlim = 15 in do
        if n > upperlim
            then let state = generate upperlim
                 in do putStrLn ("Starting game with " ++ show upperlim ++ " disks (max disks: " ++ show upperlim ++ ")")
                       delay 2
                       putStrLn ("Solvable in " ++ show (minimumSteps n) ++ " moves.")
                       delay 2
                       play state [state]
            else let state = generate n
                 in do putStrLn ("Starting game with " ++ show n ++ " disks.")
                       delay 2
                       putStrLn ("Solvable in " ++ show (minimumSteps n) ++ " moves.")
                       delay 2
                       play state [state]


-- Draw functions
-- | Prints peg to terminal at position (pivot, h)
putPeg :: Int -> Int -> IO()
putPeg _ 0 = return()
putPeg pivot h = do goto(pivot, h)
                    putStr "|"
                    putPeg pivot (h-1)

-- | Prints each row of a lists to terminal until list is empty
writeRows :: Tower -> Int -> Int -> IO()
writeRows [] _ _ = return()
writeRows (x:xs) pivot h = do goto (pivot - x, h + 1)
                              putStr (concat(replicate x " #"))
                              writeRows xs pivot (h-1)

-- | Prints a tower to the terminal
putTower :: Int -> Tower -> Int -> IO()
putTower _ _ 0 = return ()
putTower pivot xs h = do putPeg pivot (h+1)
                         writeRows xs pivot h
                         goto(0, h + 1)

-- | Calls putTower for each tower to print all towers to terminal
putTowers :: State -> IO()
putTowers [xs, ys, zs] = do clr
                            let h = maximum (xs ++ ys ++ zs)
                            putTower (2 * h) (reverse xs) h
                            putTower (4 * h) (reverse ys) h
                            putTower (6 * h) (reverse zs) h
                            goto (0, h + 2)

-- | Checks if a given move yields a valid state, prints message to screen
-- and returns value
valid :: State -> Int -> Int -> IO Bool
valid state f t
    | f < 1 || f > 3 || t < 1 || t > 3 = do putStrLn "<f> and <t> must be integers from 1 to 3."
                                            return False
    | null (state !! (f-1)) = do putStrLn ("Peg " ++ show f ++ " contains no disk.")
                                 return False
    | null (state !! (t-1)) = return True
    | head (state !! (f-1)) > head (state !! (t-1)) = do putStrLn ("Disk on " ++ show f ++ " is larger than disk on " ++ show t)
                                                         return False
    | otherwise = return True

-- | Returns a new state where first element of the first argument
-- is appended to first position of the last argument
move :: Int -> Int -> State -> State
move 1 2 = \[x:xs, ys, zs] -> [xs, x:ys, zs]
move 1 3 = \[x:xs, ys, zs] -> [xs, ys, x:zs]
move 2 1 = \[xs, y:ys, zs] -> [y:xs, ys, zs]
move 2 3 = \[xs, y:ys, zs] -> [xs, ys, y:zs]
move 3 1 = \[xs, ys, z:zs] -> [z:xs, ys, zs]
move 3 2 = \[xs, ys, z:zs] -> [xs, z:ys, zs]
move _ _  = id

-- | Returns a given amount of steps back to a previous state
undo :: Int -> [State] -> [State]
undo _ [] = []
undo n xs
    | null (tail xs) || n == 0 = xs
    | n > length xs  = undo (length xs) xs
    | otherwise      = undo (n-1) (tail xs)

-- | Prints available commands to the terminal
help :: IO()
help = putStrLn (""
    ++ "b <n> : \t start a new game with n disks\n"
    ++ "z <n> : \t undo n moves\n"
    ++ "<f> <t> : \t move disk from <f> to <t>"
    ++ "h : \t shows a hint for current game state\n"
    ++ "q : \t quits the game."
    )

-- | Checks if a given state is the finished state
finished :: State -> Bool
finished state = null (head state) && null (state !! 1)

hint :: State -> IO()
hint state = putStrLn "Not yet implemented."

play :: State -> [State] -> IO()
play state moves = do
    putTowers state
    if finished state
        then do putStrLn ("Congratulations, you won! You moved " ++ show (length (state !! 2)) ++ " disks in " ++ show (length moves - 1) ++ " moves.")
                putStrLn "Taking you back to the main menu."
                delay 7
                main
        else do
            putStrLn ("\nNumber of moves: " ++ show(length moves - 1))
            cmd <- getCmd "> "
            case words cmd of
                ["b", n] -> do isDig <- isDigit n
                               if isDig
                                   then begin (read n)
                                   else play state moves

                ["q"   ] -> do putStrLn "Stopping the program."
                               return ()

                ["z", n] -> do isDig <- isDigit n
                               if isDig
                                   then
                                       let moves' = undo (read n) moves
                                       in  play (head moves') moves'
                                   else play state moves

                ["h"   ] -> do hint state
                               delay 2
                               play state moves

                ["help"] -> do help
                               delay 3
                               play state moves

                [f, t  ] -> do isDigF <- isDigit f
                               isDigT <- isDigit t
                               if isDigF && isDigT
                                   then do
                                   val <- valid state (read f) (read t)
                                   if val
                                       then let move' = move (read f) (read t) state
                                            in  play move' (move' : moves)
                                       else do  delay 2
                                                play state moves
                               else do
                                   errCmd
                                   play state moves
                _        -> do errCmd
                               play state moves

-- IO helpers
clr :: IO()
clr = putStr "\ESC[2J"

goto :: Pos -> IO()
goto (x, y) = putStr("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeAt :: Pos -> String -> IO()
writeAt p xs = do goto p
                  putStr xs

newline :: IO()
newline = putChar '\n'

errCmd :: IO()
errCmd = do
    newline
    putStrLn "ERROR: Invalid command"
    delay 2

errMove :: IO()
errMove = do
    newline
    putStrLn "ERROR: Invalid move"
    delay 2

getCmd :: String -> IO String
getCmd prompt = do
    putStr prompt
    getLine

isDigit :: String -> IO Bool
isDigit s = do
    case readMaybe s :: Maybe Int of
        Just x  -> do return True
        Nothing -> do putStrLn "<n> must be a valid integer."
                      delay 2
                      return False