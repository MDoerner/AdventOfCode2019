module AdventOfCode20191208_2
    (
        printBIOSPassword
    ) where

import System.IO
import Data.List
import Data.List.Split
import Data.Maybe


printBIOSPassword :: IO ()
printBIOSPassword = do
    inputText <- readFile "Advent20191208_1_input.txt"
    let input = map (read . pure) inputText
    let layers = toLayers 25 6 input
    let collapsedLayer = collapseLayers 2 isNotTransparent layers
    let result = showLayer layerColour collapsedLayer
    putStrLn result

type Layer a = [[a]]

toLayers :: Int -> Int -> [a] -> [Layer a]
toLayers width height = chunksOf height . chunksOf width

transposeLayers :: [Layer a] -> Layer [a]
transposeLayers layers
    | null layers = [[]]
    | null . head $ layers = [[]]
    | null . head . head $ layers = [[]]
    | otherwise =
        let layerWidth = length . head . head $ layers
            in chunksOf layerWidth . transpose . map concat $ layers

collapseLayers :: a -> (a -> Bool) -> [Layer a] -> Layer a
collapseLayers defaultValue elementPicker layers =
    map (map (fromMaybe defaultValue . find elementPicker)) (transposeLayers layers)

isNotTransparent :: Int -> Bool
isNotTransparent = (/=) 2


data LayerColour = Black | White | Grey

showLayer :: (a -> LayerColour) -> Layer a -> String
showLayer colorPicker = unlines . map (map (charRepresentation . colorPicker))

charRepresentation :: LayerColour -> Char
charRepresentation Black = ' '
charRepresentation White = '\x2588'
charRepresentation Grey = '\x2592'

layerColour :: Int -> LayerColour
layerColour 0 = Black
layerColour 1 = White
layerColour _ = Grey




