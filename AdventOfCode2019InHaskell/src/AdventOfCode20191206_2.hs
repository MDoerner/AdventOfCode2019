module AdventOfCode20191206_2
    ( distanceToSanta
    ) where

import System.IO
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Hashable
import qualified Data.HashMap.Strict as Map


distanceToSanta :: IO ()
distanceToSanta = do
    inputText <- readFile "Advent20191206_1_input.txt"
    let orbitList = (map orbit . lines) inputText
    let orbits = orbitMap (catMaybes orbitList)
    let pathToSanta = fromJust (path orbits "COM" "YOU" "SAN")
    let requiredTransfers = length pathToSanta - 3
    print requiredTransfers


type OrbitSpecification = (String,String)
type ChildrenMap a = Map.HashMap a [a]

children :: (Eq a, Hashable a) => ChildrenMap a -> a -> [a]
children childrenMap = fromMaybe [] . flip Map.lookup childrenMap


orbit :: String -> Maybe OrbitSpecification
orbit str =
    case orbit_specification of
        [x,y] -> Just (x,y)
        _ -> Nothing
    where orbit_specification = splitOn ")" str

orbitMap :: [OrbitSpecification] -> ChildrenMap String
orbitMap = Map.fromListWith (++) . map (applyToSecondElement toSingleElementList)

applyToSecondElement :: (b -> c) -> (a,b) -> (a,c)
applyToSecondElement f (x,y) = (x, f y)

toSingleElementList :: a -> [a]
toSingleElementList x = [x]


childrenCount :: (Eq a, Hashable a) => ChildrenMap a -> a -> Int
childrenCount = childrenAggregate length

childrenAggregate :: (Eq a, Hashable a) => ([a] -> b) -> ChildrenMap a -> a -> b
childrenAggregate aggregatorFnc childrenMap = aggregatorFnc . children childrenMap

decendantCount ::  (Eq a, Hashable a) => ChildrenMap a -> a -> Int
decendantCount = decendantAggregate (+) childrenCount

decendantAggregate :: (Eq a, Hashable a) => (b -> b -> b) -> (ChildrenMap a -> a -> b) -> ChildrenMap a -> a -> b
decendantAggregate resultFoldFnc nodeFnc childrenMap node =
    foldl' resultFoldFnc nodeValue childResults
    where
        nodeValue = nodeFnc childrenMap node
        childFnc = decendantAggregate resultFoldFnc nodeFnc childrenMap
        childResults = map childFnc $ children childrenMap node

totaldecendantCount :: (Eq a, Hashable a) => ChildrenMap a -> a -> Int
totaldecendantCount = decendantAggregate (+) decendantCount


path :: (Eq a, Hashable a) => ChildrenMap a -> a -> a -> a -> Maybe [a]
path childrenMap root start end =
    let maybeStartEndPath = pathFromRoot childrenMap start end
    in if isJust maybeStartEndPath
        then maybeStartEndPath
        else let maybeEndStartPath = pathFromRoot childrenMap end start
                in case maybeEndStartPath of
                    Just endStartPath -> Just $ reverse endStartPath
                    Nothing -> let
                        rootPathToStart = pathFromRoot childrenMap root start
                        rootPathToEnd = pathFromRoot childrenMap root end
                        in if isNothing rootPathToStart || isNothing rootPathToEnd
                            then Nothing
                            else connectedPath (fromJust rootPathToStart) (fromJust rootPathToEnd)

pathFromRoot :: (Eq a, Hashable a) => ChildrenMap a -> a -> a -> Maybe [a]
pathFromRoot childrenMap root destination
    | destination == root = Just [root]
    | null childPaths = Nothing
    | otherwise = Just $ root:(head childPaths)
    where
        rootChildren = children childrenMap root
        pathFromNewRoot newRoot = pathFromRoot childrenMap newRoot destination
        childPaths = mapMaybe pathFromNewRoot rootChildren

connectedPath :: Eq a => [a] -> [a] -> Maybe [a]
connectedPath rootToStart rootToEnd =
    case pathPieces of
        Nothing -> Nothing
        Just (middle, middleToStart, middleToEnd) ->
            Just $ (reverse middleToStart) ++ [middle] ++ middleToEnd
    where pathPieces = distinctPathPieces rootToStart rootToEnd

distinctPathPieces :: Eq a => [a] -> [a] -> Maybe (a, [a], [a])
distinctPathPieces [x] [y] = if x == y then Just (x, [], []) else Nothing
distinctPathPieces (x1:y1:z1) (x2:y2:z2)
  | x1 /= x2 = Nothing
  | y1 /= y2 = Just (x1, y1:z1, y2:z2)
  | otherwise = distinctPathPieces (y1:z1) (y2:z2)
distinctPathPieces _ _ = Nothing