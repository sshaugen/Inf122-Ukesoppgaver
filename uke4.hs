-- Oppgave A

-- Listekompresjon
fjern1 :: String -> Char -> String
fjern1 xs n = [x | x <- xs, x /= n] 

--Rekursjon
fjern2 :: String -> Char -> String
fjern2 [] _ = [] 
fjern2 (x : xs) n 
    | x == n = fjern2 xs n
    | otherwise = x : fjern2 xs n


-- Oppgave B

-- Listekompresjon
tegnpos1 :: Char -> String -> [Int]
tegnpos1 n xs = [y | (x, y) <- zip xs [0..], x == n ]

-- Rekursjon
tegnpos2 :: Char -> String -> [Int]
tegnpos2 _ [] = []
tegnpos2 n xs = recursion n $ zip xs [0..]
    where
        recursion _ [] = []
        recursion b (x : xs)
            | b == fst x = snd x : recursion b xs
            | otherwise = recursion b xs
    


-- Oppgave C
intToList :: Int -> [Int]
intToList 0 = []
intToList x = intToList (x `div` 10) ++ [x `mod` 10]

-- Oppgave D(a)
settSammen :: [String] -> String
settSammen [] = ""
settSammen [x] = x
settSammen (x:xs) = x ++ " " ++ settSammen xs 

-- Oppgve D(b)

delStrengen :: String -> [String]
delStrengen "" = []
delStrengen (c : cs)    
        | [c] == " " = delStrengen cs
        | otherwise = fst removeSpace : delStrengen (snd removeSpace)
    where 
        removeSpace = break (\t -> [t] == " ") (c : cs)



-- Oppgave D(c)
gdelStrengen :: String -> String -> [String]
gdelStrengen [] _ = []
gdelStrengen (c : cs) n 
    | c `elem` n = gdelStrengen cs n
    | otherwise = fst filterString : gdelStrengen (snd filterString) n
    where 
        filterString = break (`elem` n) (c : cs)


