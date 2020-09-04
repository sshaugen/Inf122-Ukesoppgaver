
-- Oppgave 1.4
{- Ved å endre rekkefølgen på smaller og larger slik at 
larger kommer først får man en omvendt sortert liste -}
qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
                where
                    smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]


-- Oppgave 1.5
{- Dersom man erstatter "<=" med "<" vil ikke alle tallene
komme med i listen. Eksempelet qsort[2, 2, 3, 1, 1] vil man 
bare få ut en liste med [1, 2, 3] og ikke [1, 1, 2, 2, 3]-}


-- Oppgave 2.4
last' :: [a] -> a
last' [x] = x
last' xs = head (reverse xs)


-- Oppgave 2.5
init' :: [Int] -> [Int]
init' [x] = []
init' (x:xs) = x : (init' xs)
-- eller
-- init' xs = reverse (tail (reverse xs))
-- eller
-- init' xs = take (length xs - 1) xs


-- Oppgave C 
-- 1
plu :: [Int] -> Int -> [Int]
plu []_ = []
plu xs 0 = xs
plu (x:xs) n = x + n : plu xs n

-- Annen måte å gjøre det på
plu1 :: [Int] -> Int -> [Int]
plu1 es n = map (\e -> e + n) es


-- Oppgave C
-- 2
pali :: (Eq a) => [a] -> Bool
pali [] = True
pali [a] = True
pali xs = head xs == last xs && pali (init (tail xs)) 

-- Annen måte (enklere)
pali2 :: Eq a => [a] -> Bool
pali2 xs = xs == reverse xs