module AdventOfCode20191204_1
    ( simplePasswordCount
    ) where

import System.IO
import Data.List.Split
import Data.Maybe
import Text.Read


simplePasswordCount :: IO ()
simplePasswordCount = do
    let numberRange = [231832 .. 767346]
    let passwordCount = numberOfValidPasswords numberRange
    print passwordCount

numberOfValidPasswords :: [Int] -> Int
numberOfValidPasswords = length . filter isValidPassword

isValidPassword :: Int -> Bool
isValidPassword password = isAscending digits && hasDouble digits
    where digits = toDigits password

toDigits ::  Integral x => x -> [x]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

isAscending :: [Int] -> Bool
isAscending [] = True
isAscending [x] = True
isAscending (x:(y:remainder)) = x <= y && isAscending (y:remainder)

hasDouble :: [Int] -> Bool
hasDouble [] = False
hasDouble [x] = False
hasDouble (x:(y:remainder)) = x == y || hasDouble (y:remainder)