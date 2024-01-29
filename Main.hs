module Main where
import System.Environment
import System.Exit
import Data.Char
import Operations (addition, substraction, multiply, divide, doop)

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt a | all isDigit a == True = Just (read a)
          | head a == '-' = Just (read a)
          | otherwise = Nothing

myFromJust :: Maybe Int -> Int
myFromJust Nothing = 0
myFromJust (Just a) =  a

main::IO ()
main = do
    x <- getArgs
    let a = myFromJust(readInt(x!!0))
    let b = (x!!1)
    let c = myFromJust(readInt(x!!2))
    doopResult <- doop a (head b) c
    print doopResult