clr :: IO ()
clr = putStr "\ESC[2J"

goto :: (Int, Int) -> IO ()
goto (x, y) = putStr("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeAt :: (Int, Int) -> String -> IO ()
writeAt (x, y) xs = do goto(x, y)
                       putStrLn xs


-- Oppgave C
writeRows :: Int -> (Int, Int) -> IO ()
writeRows n (x, y) = 
        if n == 0 then return()
        else do 
                writeAt(x, y) (concat(replicate n "* "))
                writeRows (n - 1) (x + 1, y - 1)

trekant :: Int -> (Int, Int) -> IO()
trekant n (x, y) = do 
        writeRows n (x, y)
        goto(0, n + 1)

trekanter :: Int -> Int -> Int -> IO()
trekanter x y z = let n = maximum [x, y, z] in do
        clr
        trekant x (1, n)
        trekant y ((x*2) + 2, n)
        trekant z ((x*2)+(y*2) + 3, n)
        goto(0, maximum [x, y, z] + 1)

trekanter' :: Int -> Int -> Int -> IO()
trekanter' x y z = 
        let height = maximum [x, y, z]
            length = (x + y + z + 3) * 2
            counter = height
        in trekanterhelper x y z height length counter

trekanterhelper x y z height length counter =
        if counter == 0 
                then return ()
        else do
        putStr (rad x (x*2) counter)
        putStr (rad y (y*2) counter)
        putStr (rad z (z*2) counter)
        putStr "\n"
        trekanterhelper x y z height length (counter - 1)

rad x length counter = if x < counter then concat(replicate length "s ")
        else concat (replicate (length - x) "s") ++ concat(replicate x "* ") ++ concat(replicate (length - x) "s")

-- 3 3 3 3 3 3 3 4 4 4 4 4 4 5 5 * 5 5
-- 3 3 3 3 3 3 3 4 4 4*4 4 4 5 5* *5 5
-- 3 3 3 * 3 3 3 4 4 * * 4 4 5 * * * 5
-- 3 3 3* *3 3 3 4 4* * *4 4 5* * * *5
-- 3 3 * * * 3 3 4 * * * * 4 * * * * * 

-- Oppgave D
data FileOrFolder = File Int | Folder [FileOrFolder]

prettyPrint :: FileOrFolder -> IO()
prettyPrint (File x) = putStrLn ("-File" ++ show x)
prettyPrint (Folder xs) = do 
        clr
        printFolder (length xs) 0
        mapM_ (\x -> helper x 1) xs

helper :: FileOrFolder -> Int -> IO()
helper (File x) idx = printFile x idx
helper (Folder xs) idx = do
        printFolder (length xs) idx
        mapM_ (\x -> helper x (idx+1)) xs

printFolder :: Int -> Int -> IO()
printFolder x i = putStrLn (indent i ++ "-Folder" ++ " " ++ show x)

printFile :: Int -> Int -> IO()
printFile x i = putStrLn (indent i ++ "-File" ++ " " ++ show x)

indent :: Int -> String
indent i = concat (replicate (i*2) " ")