-- Del A

-- Oppgave 7.1
func1 :: [t] -> (t -> a) -> (t -> Bool) -> [a]
func1 xs f p = [f x | x <- xs, p x]

func2 :: [a] -> (a -> b) -> (a -> Bool) -> [b]
func2 xs f p = map f (filter p xs)

-- Oppgave 7.4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0

-- Oppgave 7.5
-- curry :: ((a, b) -> c) -> a -> b -> c
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \x y -> f (x, y)

-- uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry' :: (a -> b -> c ) -> (a, b) -> c
uncurry' f = \(x,y) -> f x y

-- Oppgave 7.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap x _ [a] = [x a]
altMap x y (a : b : xs) = x a : y b : altMap x y xs

-- Oppgave 8.5
data Expr = Val Int | Add Expr Expr deriving (Show)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- Oppgave 8.6
eval :: Expr -> Int
eval = folde (\x -> x) (\x -> \y -> x + y)

size :: Expr -> Int
size = folde (\a -> 1) (\x -> \y -> x + y)

-- Del B
prefiks :: Expr -> String
prefiks(Val n) = show n ++ " " 
prefiks (Add a b) = "+ " ++ prefiks a ++ prefiks b

infiks :: Expr -> String
infiks (Val n) = show n
infiks (Add a b) = "(" ++ infiks a ++ " + " ++ infiks b ++ ")"

postfiks :: Expr -> String
postfiks (Val n) = " " ++ show n 
postfiks (Add a b) = postfiks a ++ postfiks b ++ " +"

