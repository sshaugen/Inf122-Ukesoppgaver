import System.IO
import Data.Char

-- Oppgave A 
brett :: Int -> IO()
brett n = do
    clear'
    top n
    mapM_ (\i -> row i n) [1..n]

clear' :: IO()
clear' = putStr"\ESC[2J"

pos = 3
top :: (Show a, Integral a) => a -> IO()
top n = 
    writeAt (pos + 1, 0) ((concat [(show (mod i 10)) ++ " " | i <- [1..n]]) ++ "\n")

writeAt :: (Int, Int) -> String -> IO()
writeAt (x, y) xs = do
    goto (x,y)
    putStr xs

goto :: (Int, Int) -> IO()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

row :: Int -> Int -> IO()
row i n = do
    writeAt (if i > 9 then pos - 2 else pos - 1, 1 + i) (show i)
    mapM_ (\i -> putStr " .") [1..n]
    putStrLn ""

-- Oppgave B
brett' :: Int -> IO()
brett' n = do
    brett n 
    putStrLn "Skriv inn en kommando:"
    clearMessage n
    repeat' n

repeat' :: Int -> IO()
repeat' rows = do
    w <- getCommando
    let cmd = words w 
    command cmd rows

command :: [String] -> Int -> IO()
command cmd row 
    | head cmd == "q" = return ()
    | head cmd == "n" = do
        acceptable cmd row
        writeAt (pos' cmd) "X"
        clearMessage row
        repeat' row
    | head cmd == "d" = do
        acceptable cmd row
        writeAt (pos' cmd) "."
        clearMessage row
        repeat' row
    | otherwise = do 
        print "Ukjent kommando"
        repeat' row

getCommando :: IO String
getCommando = do 
    l <- getLine
    if length l /= 0 then return l else getCommando

pos' :: [String] -> (Int, Int)
pos' cmd = (x + (x - 1) + 3, y + 1)
            where
                x = read (cmd !! 1)
                y = read (cmd !! 2)

clearLine :: (Int, Int) -> IO ()
clearLine (x, y) = do
  goto (x, y)
  putStr "\ESC[2K"

clearMessage :: Int -> IO ()
clearMessage n = do
  clearLine (0, n + 3)
  clearLine(0, n + 4)

acceptable :: [String] -> Int -> IO()
acceptable cmd row  
    | x > row = do
        print "Utenfor brettet"
        clearMessage row
        repeat' row
    | y > row = do
        print "Utenfor brettet"
        clearMessage row
        repeat' row
    | x < 1 = do
        print "Utenfor brettet"
        clearMessage row
        repeat' row
    | y < 1 = do
        print "Utenfor brettet"
        clearMessage row
        repeat' row
    | otherwise = return()
    where
        x = read (cmd !! 1)
        y = read (cmd !! 2)