module AdventOfCode20191208_1
    (
        minZeroDigitCountProduct
    ) where

import System.IO
import Data.List
import Data.List.Split


minZeroDigitCountProduct :: IO ()
minZeroDigitCountProduct = do
    inputText <- readFile "Advent20191208_1_input.txt"
    let input = map (read . pure) inputText
    let layers = toLayers 25 6 input
    let resultLayer = leastOccurrencesLayer 0 layers
    let result = elemCountProduct 1 2 resultLayer
    print result

type Layer a = [[a]]

toLayers :: Int -> Int -> [a] -> [Layer a]
toLayers width height = chunksOf height . chunksOf width

countOccurrences :: (Eq a) => a -> Layer a -> Int
countOccurrences elem layer = sum $ map (\x -> if x == elem then 1 else 0) (concat layer)

leastOccurrencesLayer :: (Eq a) => a -> [Layer a] -> Layer a
leastOccurrencesLayer elem =
    minimumBy (\x y -> compare (countOccurrences elem x) (countOccurrences elem y))

elemCountProduct :: (Eq a) => a -> a -> Layer a -> Int
elemCountProduct elem1 elem2 layer = countOccurrences elem1 layer * countOccurrences elem2 layer