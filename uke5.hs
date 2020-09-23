import Data.Char


-- Oppgave 1
data Ast = V Int | P Ast Ast | M Ast Ast |S String deriving Show

eval :: Ast -> Int
eval (V x) = x
eval (P a b) = (eval a) + (eval b)
eval (M a b) = (eval a) * (eval b)


-- Oppgave 2
inn :: Ast -> String
inn (V x) = show x
inn (P a b) = "(" ++ (inn a) ++ "+" ++ (inn b) ++ ")"
inn (M a b) = "(" ++ (inn a) ++ "*" ++ (inn b) ++ ")"


-- Oppgave 3.1 
tokenize :: String -> [String]
tokenize "" = []
tokenize (x:xs)  
    | elem x t  = [x] : tokenize xs 
    | elem x s = tokenize xs 
    | otherwise = (takeWhile (notin $ t++s) (x:xs)) :
                 tokenize (dropWhile (notin $ t++s) (x:xs))
    where 
        t = "*+()"
        s = " "
        notin xs = (\x -> not (elem x xs))

-- Oppgave 3.2
parse :: String -> Ast
parse x = fst (parse' (tokenize x))

parse' :: [String] -> (Ast, [String])
parse' [] = error "For få tall"
parse' ("+":xs) =
        let (e1, r1) = parse' xs 
            (e2, r2) = parse' r1 
        in (P e1 e2, r2)
parse' ("*":xs) =
        let (e1, r1) = parse' xs 
            (e2, r2) = parse' r1 
        in (M e1 e2, r2)
parse' (x:xs) = if
                    (onlyDigits x)
                        then
                             (V (read x :: Int), xs)
                else
                    (S x, xs)
onlyDigits xs = takeWhile isDigit xs == xs

-- Oppgave 3.3 
ev :: String -> Int 
ev str = eval $ parse str


-- Oppgave 3.4 
innfix :: String -> String 
innfix str = tail ( init (inn ( parse str) ) )
    