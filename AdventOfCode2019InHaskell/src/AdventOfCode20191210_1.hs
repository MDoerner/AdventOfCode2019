module AdventOfCode20191210_1
    (
        bestLocation,
        mostAsteroidsVisible
    ) where

import System.IO
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Set as Set


mostAsteroidsVisible :: IO ()
mostAsteroidsVisible = do
    inputText <- readFile "Advent20191210_1_input.txt"
    let asteroids = toPoints inputText
    let mostOthersVisible = mostVisibleAsteroids asteroids - 1
    print mostOthersVisible


bestLocation :: IO ()
bestLocation = do
    inputText <- readFile "Advent20191210_1_input.txt"
    let asteroids = toPoints inputText
    let loactionWithBestView = locationWithMostVisibleAsteroids asteroids
    print loactionWithBestView




type Point = (Double, Double)
type Vector = Point
type Direction = Vector

-- Input

toPoints :: String -> [Point]
toPoints = map fst . filter (isAsteroid . snd) . inputWithCoordinates . lines

inputWithCoordinates :: [[a]] -> [(Point, a)]
inputWithCoordinates input = zip (inputCoordinates input) (concat input)

inputCoordinates :: [[a]] -> [Point]
inputCoordinates [] = []
inputCoordinates input = [(fromIntegral x, fromIntegral y) | y <- [0..(inputHeight input - 1)], x <- [0..(inputWidth input - 1)]]

inputWidth :: [[a]] -> Int
inputWidth [] = 0
inputWidth input =  length . head $ input

inputHeight :: [[a]] -> Int
inputHeight [] = 0
inputHeight input = length input

isAsteroid :: Char -> Bool
isAsteroid '#' = True
isAsteroid _ = False


-- Computation

l1Norm :: Vector -> Double
l1Norm (x,y) = abs x + abs y

direction :: Vector -> Direction
direction (0,0) = (0,0)
direction point@(x,y) = let norm = l1Norm point
                            in (x/norm , y/norm)

difference :: Point -> Point -> Vector
difference (x1,y1) (x2,y2) = (x2-x1, y2-y1)

numberOfDistinctElements :: Ord a => [a] -> Int
numberOfDistinctElements = Set.size . Set.fromList

numberOfVisibleAsteroids :: [Point] -> Point -> Int
numberOfVisibleAsteroids asteroids basePoint = numberOfDistinctElements $ map (direction . difference basePoint) asteroids

mostVisibleAsteroids :: [Point] -> Int
mostVisibleAsteroids asteroids = maximum $ map (numberOfVisibleAsteroids asteroids) asteroids

locationWithMostVisibleAsteroids :: [Point] -> Point
locationWithMostVisibleAsteroids asteroids =
    maximumBy (\x y -> compare (numberOfVisibleAsteroids asteroids x) (numberOfVisibleAsteroids asteroids y)) asteroids