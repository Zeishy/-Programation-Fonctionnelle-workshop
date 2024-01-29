import System.Environment
import System.Exit
import Data.Char

isOperator :: String -> Bool
isOperator [] = error "Empty string"
isOperator s = s `elem` ["+", "-", "/", "*"]

isNum :: String -> Bool
isNum [] = error "Empty String"
isNum s = all isDigit s

filterArr :: (a -> Bool) -> [a] -> [a]
filterArr a [] = []
filterArr a (b:bs) | a b = b : filterArr a bs
                   | otherwise = filterArr a bs

parseLine :: [String] -> ([String], [String])
parseLine [] = ([], [])
parseLine (x:xs) | isOperator x == True = (x : fst(parseLine xs), snd(parseLine xs)) 
                 | isNum x == True = (fst(parseLine xs), x: snd(parseLine xs))
                 | otherwise = ([], [])