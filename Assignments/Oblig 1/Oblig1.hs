-- Anders Tuft Buanes
module Oblig1 where
import Graphics.Win32.GDI (sYSTEM_FONT)
import Data.Array.IArray (accum)

dictionary = [
        ("bb",["Big Brother"]),
        ("dep",["department"]),
        ("sec", ["Sector"]),
        ("doubleplusgood",["excellent", "fabulous", "fantastic", "best"]),
        ("doubleplusungood", ["terrible", "horrible", "worst"]),
        ("Ingsoc", ["English Socialism"]),
        ("joycamp", ["labour camp"]),
        ("Oldspeak", ["Standard English", "English"]),
        ("oldthink", ["objectivity", "rationalism", "democracy"]),
        ("thinkpol", ["The Thought Police"]),
        ("prolefeed", ["Popular culture", "pop-culture"]),
        ("crimethink", ["liberty", "equality", "privacy", "thoughtcrime"]),
        ("fullwise", ["fully", "completely", "totally"]),
        ("goodthink", ["political orthodoxy", "politically orthodox thought", "orthodox thought"]),
        ("goodwise", ["well"]),
        ("ownlife", ["anti-social tendency", "solitude", "individualism"]),
        ("plusgood", ["very good", "great"]),
        ("plusungood", ["very bad"]),
        ("misprint", ["error", "misprediction"]),
        ("Miniluv", ["The Ministry of Love"]),
        ("Minipax", ["The Ministry of Peace"]),
        ("Minitrue", ["The Ministry of Truth"]),
        ("Miniplenty", ["The Ministry of Plenty"]),
        ("bellyfeel", ["blind, enthusiastic acceptance"]),
        ("doublethink", ["believing two contradictory ideas"]),
        ("duckspeak", ["vocal support of political orthodoxies"]),
        ("un", ["not"]),
        ("peace", ["war"]),
        ("strength", ["ignorance"]),
        -- The next line contains a list of forbidden words that don't have a translation to Newspeak, these should be replaced with '*'s
        ("",["freedom", "revolution", "fun", "diary", "surveillance", "Great Britain", "Winston Smith", "Julia"])
        ]


-- Oppgave 1 ----------------------------------------------------
isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys)
        | x == y        = isPrefix xs ys
        | otherwise     = False


isPrefix' :: String -> String -> Bool
isPrefix' xs ys = take (length xs) ys == xs


-- Oppgave 2 ----------------------------------------------------
locate :: String -> String -> [(Int,Int)]
locate xs ys = idx xs ys [] 0
        where  idx xs ys acc n
                | null ys = acc
                | null xs = []
                | isPrefix xs ys = acc ++ [(n, n + length xs)] ++ idx xs (tail ys) acc (n + 1)
                | otherwise = idx xs (tail ys) acc (n + 1)


-- Oppgave 3 ----------------------------------------------------
translate :: String -> String
translate xs = traverse xs dictionary
        where traverse xs dict
                | null dict = []
                | any (\x -> isPrefix xs x && length x == length xs) (snd(head dict)) = fst (head dict)
                | otherwise = traverse xs (tail dict)

-- Oppgave 4 ----------------------------------------------------
replace :: [(Int, Int)] -> String -> String
replace _ [] = []
replace [] ys = ys
replace xs ys = helper (qsort xs) ys
        where helper (x:xs) ys
                | null (translate (substring (fst x) (snd x) ys)) = replace xs (substring 0 (fst x) ys ++ ast (snd x - fst x) ++ substring (snd x) (length ys) ys)
                | otherwise                                       = replace xs (substring 0 (fst x) ys ++ translate (substring (fst x) (snd x) ys) ++ substring (snd x) (length ys) ys)
                where
                        substring i j xs = take (j - i) (drop i xs)
                        ast i = concat (replicate i "*")

qsort :: [(Int,Int)] -> [(Int,Int)]
qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
        where
                smaller = [a | a <- xs, a <= x]
                larger = [b | b <- xs, b > x]


-- Oppgave 5 ----------------------------------------------------
toNewspeak :: String -> String
toNewspeak xs = replace (idcs xs (concatMap snd dictionary)) xs
        where idcs xs dictionary
                | null dictionary = []
                | otherwise = locate (head dictionary) xs ++ idcs xs (tail dictionary)


-- Oppgave 6 ----------------------------------------------------
analytics :: String -> String -> Int
analytics xs ys = helper xs ys
        where helper xs ys = round(100 * (fromIntegral(notIn xs ys 0) / fromIntegral(length xs)))

notIn :: String -> String -> Int -> Int
notIn [] ys n = n
notIn xs [] n = length xs
notIn (x:xs) ys n
        | x `elem` ys = notIn xs (deleteFirst x ys) n
        | otherwise = notIn xs ys (n + 1)

deleteFirst :: Char -> String -> String
deleteFirst _ [] = []
deleteFirst x (y:ys)
        | x == y = ys
        | otherwise = y : deleteFirst x ys
        

-- Oppgave 7 ----------------------------------------------------
main :: String -> (String, Int)
main xs = let ys = toNewspeak xs in (ys, analytics xs ys)