module AdventOfCode20191204_2
    ( aLittlLessSimplePasswordCount
    ) where

import System.IO
import Data.List.Split
import Data.Maybe
import Text.Read


aLittlLessSimplePasswordCount :: IO ()
aLittlLessSimplePasswordCount = do
    let numberRange = [231832 .. 767346]
    let passwordCount = numberOfValidPasswords numberRange
    print passwordCount

numberOfValidPasswords :: [Int] -> Int
numberOfValidPasswords = length . filter isValidPassword

isValidPassword :: Int -> Bool
isValidPassword password = isAscending digits && hasIsolatedDouble digits
    where digits = toDigits password

toDigits ::  Integral x => x -> [x]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

isAscending :: [Int] -> Bool
isAscending [] = True
isAscending [x] = True
isAscending (x:(y:remainder)) = x <= y && isAscending (y:remainder)

hasIsolatedDouble :: [Int] -> Bool
hasIsolatedDouble = hasIsolatedDoublePreviousDifferent

hasIsolatedDoublePreviousDifferent :: [Int] -> Bool
hasIsolatedDoublePreviousDifferent [] = False
hasIsolatedDoublePreviousDifferent [x] = False
hasIsolatedDoublePreviousDifferent [x, y] = x == y
hasIsolatedDoublePreviousDifferent [x, y, z] = x == y && y /= z || x /= y && y == z
hasIsolatedDoublePreviousDifferent (x:(y:(z:remainder))) = 
    x == y && (y /= z || hasIsolatedDoublePreviousMultipleSame (z:remainder))
        || x /= y && hasIsolatedDoublePreviousDifferent (y:(z:remainder))

hasIsolatedDoublePreviousMultipleSame :: [Int] -> Bool
hasIsolatedDoublePreviousMultipleSame [] = False
hasIsolatedDoublePreviousMultipleSame [x] = False
hasIsolatedDoublePreviousMultipleSame [x, y] = False 
hasIsolatedDoublePreviousMultipleSame [x, y, z] = x /= y && y == z 
hasIsolatedDoublePreviousMultipleSame (x:(y:(z:remainder))) = 
    x /= y && hasIsolatedDoublePreviousDifferent (y:(z:remainder)) 
        || x == y && hasIsolatedDoublePreviousMultipleSame (y:(z:remainder))