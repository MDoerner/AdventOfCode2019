module AdventOfCode20191206_2
    ( distanceToSanta
    ) where

import System.IO
import Data.List.Split
import Data.Maybe
import Text.Read
import Control.Monad
import Data.Hashable
import qualified Data.HashMap.Strict as Map

type OrbitSpecification = (String,String)

distanceToSanta :: IO ()
distanceToSanta = do
    inputText <- readFile "Advent20191206_1_input.txt"
    let orbitList = (map orbit . lines) inputText
    let orbits = orbitMap (catMaybes orbitList)
    let pathToSanta = fromJust (path orbits "COM" "YOU" "SAN")
    let requiredTransfers = length pathToSanta - 3
    print requiredTransfers

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

pathFromRoot :: (Eq a, Hashable a) => Map.HashMap a [a] -> a -> a -> Maybe [a]
pathFromRoot orbits destination root
    | destination == root = Just [root]
    | null childPaths = Nothing
    | otherwise = Just (root:(head childPaths))
    where
        maybeChildren = Map.lookup root orbits
        childPaths = case maybeChildren of
                        Nothing -> []
                        Just children -> mapMaybe (pathFromRoot orbits destination) children

path :: (Eq a, Hashable a) => Map.HashMap a [a] -> a -> a -> a -> Maybe [a]
path orbits root start end =
    let maybeStartEndPath = pathFromRoot orbits end start
    in if isJust maybeStartEndPath
        then maybeStartEndPath
        else let maybeEndStartPath = pathFromRoot orbits start end
                in case maybeEndStartPath of
                    Just endStartPath -> Just (reverse endStartPath)
                    Nothing -> let
                        rootPathToStart = pathFromRoot orbits start root
                        rootPathToEnd = pathFromRoot orbits end root
                        in if isNothing rootPathToStart || isNothing rootPathToEnd
                            then Nothing
                            else connectedPath (fromJust rootPathToStart) (fromJust rootPathToEnd)

connectedPath :: Eq a => [a] -> [a] -> Maybe [a]
connectedPath rootToStart rootToEnd =
    if isJust middle
        then Just ((reverse middleToStart) ++ [fromJust middle] ++ middleToEnd)
        else Nothing
    where (middle, middleToStart, middleToEnd) = distinctPathPieces rootToStart rootToEnd

distinctPathPieces :: Eq a => [a] -> [a] -> (Maybe a, [a], [a])
distinctPathPieces [x] [y] = if x == y then (Just x, [], []) else (Nothing, [], [])
distinctPathPieces (x1:(y1:z1)) (x2:(y2:z2))
  | x1 /= x2 = (Nothing, [], [])
  | y1 /= y2 = (Just x1, y1:z1, y2:z2)
  | otherwise = distinctPathPieces (y1:z1) (y2:z2)
distinctPathPieces _ _ = (Nothing, [], [])




