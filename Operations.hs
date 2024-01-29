module Operations where
import System.Environment
import System.Exit

addition :: Int -> Int -> Int
addition a b = a + b

substraction :: Int -> Int -> Int
substraction a b = a - b

multiply :: Int -> Int -> Int
multiply a b = a * b

divide :: Int -> Int -> Maybe Int
divide _ 0 = Nothing
divide 0 _ = Just (0)
divide a b = Just (div a b)

doop :: Int -> Char -> Int -> IO Int
doop _ '/' 0 = exitWith(ExitFailure 84)
doop _ '%' 0 = exitWith(ExitFailure 84)
doop x '-' y = return (x - y)
doop x '+' y = return (x + y)
doop x '*' y = return (x * y)
doop x '/' y = return (x `div` y)
doop x '%' y = return (x `mod` y)