import Data.List

-- Oppgave 4.5
(&&) :: Bool -> Bool -> Bool 
x && y = if x == True
                then if (y == True) then True else False
            else False

-- Oppgave 4.7
mult :: Int -> Int -> Int -> Int
mult = (\x y z -> x * y * z)

-- Oppgave 5.6
factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects x = [x | x <- [1..x], sum (factors x) == 2 * x]


-- Oppgave 5.7
singlegen :: [(Integer, Integer)]
singlegen = concat [[(x,y) | x <- [1,2]] | y <- [3,4]]

-- Oppgave 5.9 
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct x y = sum [ a * b | (a, b) <- zip x y]

-- Oppgave C
rem1 :: Eq a => [a] -> a -> [a]
rem1 [] _ = []
rem1 (x:xs) n = if x == n
                    then xs
                else x : rem1 xs n

-- Oppgave D
diff :: Eq a => [a] -> [a] -> [a]
diff a []  = a
diff [] b = []
diff a b = a \\ b