module AdventOfCode20191210_2
    (
        ateroid200
    ) where

import System.IO
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as Map
import Debug.Trace


ateroid200 :: IO ()
ateroid200 = do
    inputText <- readFile "Advent20191210_1_input.txt"
    let asteroids = toPoints inputText
    let loactionWithBestView = locationWithMostVisibleAsteroids asteroids
    let orderOfDestruction = destructionOrder loactionWithBestView asteroids
    let (x,y) = orderOfDestruction!!199
    print (100 * x + y)



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

differenceDirection :: Point -> Point -> Direction
differenceDirection basePoint = direction . difference basePoint

differenceNorm :: Point -> Point -> Double
differenceNorm basePoint = l1Norm . difference basePoint


numberOfDistinctElements :: Ord a => [a] -> Int
numberOfDistinctElements = Set.size . Set.fromList

numberOfVisibleAsteroids :: [Point] -> Point -> Int
numberOfVisibleAsteroids asteroids basePoint = numberOfDistinctElements $ map (differenceDirection basePoint) asteroids

mostVisibleAsteroids :: [Point] -> Int
mostVisibleAsteroids asteroids = maximum $ map (numberOfVisibleAsteroids asteroids) asteroids

locationWithMostVisibleAsteroids :: [Point] -> Point
locationWithMostVisibleAsteroids asteroids =
    maximumBy (\x y -> compare (numberOfVisibleAsteroids asteroids x) (numberOfVisibleAsteroids asteroids y)) asteroids

otherAsteroidsByDirection :: Point -> [Point] -> Map.HashMap Direction [Point]
otherAsteroidsByDirection basePoint asteroids = Map.delete (0,0) (Map.fromListWith (++) $ zip (map (differenceDirection basePoint) asteroids) (map pure asteroids))

eachOrderedByDifferenceNorm :: Point -> Map.HashMap k [Point] -> Map.HashMap k [Point]
eachOrderedByDifferenceNorm basePoint = Map.map (sortOn $ differenceNorm basePoint)

directionOrder :: Direction -> Double
directionOrder (0,0) = -1
directionOrder (x,y) = if x >= 0
                        then y + 1
                        else 3 - y

orderedByDirection :: Map.HashMap Direction [Point] -> [[Point]]
orderedByDirection points = map (fromJust . flip Map.lookup points) (sortOn directionOrder (Map.keys points))

destructionOrder :: Point -> [Point] -> [Point]
destructionOrder basePoint = concat . transpose . orderedByDirection . (eachOrderedByDifferenceNorm basePoint) . (otherAsteroidsByDirection basePoint)