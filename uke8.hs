-- Oppgave 1

--Rekursiv
al1 :: [Bool] -> Bool 
al1 [] = True
al1 (x:xs) = x && al1 xs

-- Passende innebygget funksjon
al2 :: [Bool] -> Bool
al2 = all (== True)

-- Foldl
al3 :: [Bool] -> Bool
al3 = foldl (\x y -> x && y) True

-- Foldr
al4 :: [Bool] -> Bool
al4 = foldr (&&) True

-- Oppgave 2
ala :: (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool
ala a b = foldr a b

-- Oppgave 3
trekant :: Int -> IO ()
trekant x = putStr (tegn 1 x)

tegn :: Int -> Int -> String
tegn a b 
    | a == b = symbol "* " a ++ "\n"
    | otherwise = symbol "* " a ++ "\n" ++ tegn (a + 1) b

symbol :: (Eq t, Num t) => [a] -> t -> [a]
symbol _ 0 = []
symbol x y = x ++ symbol x (y - 1)

-- Oppgave 4

juletre :: Int -> IO()
juletre t = putStr(tegn' 1 t)

tegn' :: Int -> Int -> String
tegn' a b 
    | a == b = symbol " " (b - a) ++ symbol " *" a ++ "\n"
    | otherwise = symbol " " (b - a) ++ symbol " *" a ++ "\n" ++ tegn' (a + 1) b