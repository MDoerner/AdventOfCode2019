module AdventOfCode20191206_1
    ( orbitCount
    ) where

import System.IO
import Data.List.Split
import Data.Maybe
import Text.Read
import Control.Monad
import Data.Hashable
import qualified Data.HashMap.Strict as Map

type OrbitSpecification = (String,String)

orbitCount :: IO ()
orbitCount = do
    inputText <- readFile "Advent20191206_1_input.txt"
    let orbitList = (map orbit . lines) inputText
    let orbits = orbitMap (catMaybes orbitList)
    let totalOrbits = totalDecendentCount orbits "COM"
    print totalOrbits

orbit :: String -> Maybe OrbitSpecification
orbit str =
    case orbit_specification of
        [x,y] -> Just (x,y)
        _ -> Nothing
    where orbit_specification = splitOn ")" str

orbitMap :: [OrbitSpecification] -> Map.HashMap String [String]
orbitMap = Map.fromListWith (++) . map (applyToSecondElement toSingleElementList)

applyToSecondElement :: (b -> c) -> (a,b) -> (a,c)
applyToSecondElement f (x,y) = (x, f y)

toSingleElementList :: a -> [a]
toSingleElementList x = [x]

childOrbitCount :: (Eq a, Hashable a) => Map.HashMap a [a] -> a -> Int
childOrbitCount = orbitAggregate 0 length

orbitAggregate :: (Eq a, Hashable a) => b -> ([a] -> b) -> Map.HashMap a [a] -> a -> b
orbitAggregate defaultValue aggregatorFnc orbits orbitted =
    case maybeOrbitting of
        Nothing -> defaultValue
        Just orbitting -> aggregatorFnc orbitting
    where maybeOrbitting = Map.lookup orbitted orbits

decendentOrbitCount ::  (Eq a, Hashable a) => Map.HashMap a [a] -> a -> Int
decendentOrbitCount = orbitMapAggregate 0 childOrbitCount

orbitMapAggregate :: (Eq a, Hashable a, Num b) => b -> (Map.HashMap a [a] -> a -> b) -> Map.HashMap a [a] -> a -> b
orbitMapAggregate defaultValue aggregator orbits orbitted =
    aggregator orbits orbitted + orbitAggregate defaultValue (sum . map (orbitMapAggregate defaultValue aggregator orbits)) orbits orbitted

totalDecendentCount :: (Eq a, Hashable a) => Map.HashMap a [a] -> a -> Int
totalDecendentCount = orbitMapAggregate 0 decendentOrbitCount