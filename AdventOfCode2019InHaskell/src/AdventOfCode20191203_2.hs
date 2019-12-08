module AdventOfCode20191203_2
    ( intersectionWireDistance
    ) where

import System.IO
import Data.List.Split
import Data.Maybe
import Text.Read


intersectionWireDistance :: IO ()
intersectionWireDistance = do
    inputText <- readFile "Advent20191203_1_input.txt"
    let relativeWireSpecifications = toRelativeWires inputText
    let absoluteWires = map (toAbsoluteWire (Point 0 0)) relativeWireSpecifications
    let minimalDistance = minimalIntersectionNorm (wireDistance absoluteWires) (Point 0 0) absoluteWires
    print minimalDistance

toRelativeWires :: String -> [RelativeWire]
toRelativeWires = map toRelativeWire . lines

toRelativeWire :: String -> RelativeWire
toRelativeWire = mapMaybe toRelativeSection . splitOn ","

minimalIntersectionNorm :: (Point -> Maybe Int) -> Point -> [AbsoluteWire] -> Maybe Int
minimalIntersectionNorm _ _ [] = Nothing
minimalIntersectionNorm _ _ [x] = Nothing
minimalIntersectionNorm norm start (x:(y:_)) =
    (Just . minimum) (mapMaybe norm intersectionPoints)
    where intersectionPoints = filter ((/=) start) (wireIntersections x y)


data Direction = LEFT | RIGHT | UP | DOWN deriving(Eq, Show)

toDirection :: Char -> Maybe Direction
toDirection 'L' = Just LEFT
toDirection 'R' = Just RIGHT
toDirection 'U' = Just UP
toDirection 'D' = Just DOWN
toDirection _ = Nothing


type RelativeSection = (Direction, Int)
type RelativeWire = [RelativeSection]

toRelativeSection :: String -> Maybe RelativeSection
toRelativeSection [] = Nothing
toRelativeSection (c:digits)
    | isNothing maybeDirection = Nothing
    | isNothing maybeDistance = Nothing
    | otherwise = Just (fromJust maybeDirection, fromJust maybeDistance)
    where
        maybeDirection = toDirection c
        maybeDistance = readMaybe digits

data Point = Point {
     xCoord :: Int,
     yCoord :: Int
} deriving(Eq, Show)

manhattenNorm :: Point -> Int
manhattenNorm = manhattenDistance (Point 0 0)

manhattenDistance :: Point -> Point -> Int
manhattenDistance (Point x1 y1) (Point x2 y2) = abs (x2 - x1) + abs (y2 - y1)

singleWireDistance :: AbsoluteWire -> Point -> Maybe Int
singleWireDistance [] _ = Nothing
singleWireDistance (section:remainder) point =
    if isOnSection point section
        then Just (manhattenDistance (startPoint section) point)
        else let remainingDistance = singleWireDistance remainder point
                in case remainingDistance of
                    Nothing -> Nothing
                    Just distance -> Just (distance + manhattenDistance (startPoint section) (endPoint section))

wireDistance :: [AbsoluteWire] -> Point -> Maybe Int
wireDistance wires point = 
    if null individualWireDistances 
        then Nothing 
        else Just (sum individualWireDistances)
    where individualWireDistances = mapMaybe (`singleWireDistance` point) wires

data AbsoluteSection = AbsoluteSection {
    startPoint :: Point,
    endPoint :: Point
} deriving(Eq, Show)

isHorizontal :: AbsoluteSection -> Bool
isHorizontal (AbsoluteSection (Point _ y1) (Point _ y2)) = y1 == y2

isVertical :: AbsoluteSection -> Bool
isVertical (AbsoluteSection (Point x1 _) (Point x2 _)) = x1 == x2

isOnSection :: Point -> AbsoluteSection -> Bool
isOnSection point (AbsoluteSection start end) =
    (xCoord start <= xCoord point && xCoord point <= xCoord end
        ||  xCoord start >= xCoord point && xCoord point >= xCoord end)
    && (yCoord start <= yCoord point && yCoord point <= yCoord end
        ||  yCoord start >= yCoord point && yCoord point >= yCoord end)

intersectionPoint :: AbsoluteSection -> AbsoluteSection -> Maybe Point
intersectionPoint section1 section2
        | not (isHorizontal section1) && not (isHorizontal section2) = Nothing
        | not (isHorizontal section1) = intersectionPoint section2 section1
        | not (isVertical section2) = Nothing
        | otherwise = let potentialIntersection = Point (xCoord (startPoint section2)) (yCoord (startPoint section1))
                        in if isOnSection potentialIntersection section1
                            && isOnSection potentialIntersection section2
                           then Just potentialIntersection
                           else Nothing


type AbsoluteWire = [AbsoluteSection]

toAbsoluteWire :: Point -> RelativeWire -> AbsoluteWire
toAbsoluteWire _ [] = []
toAbsoluteWire start (firstWire:rest) =
    let end = sectionEndPoint start firstWire
        currentSection = AbsoluteSection start end
        in currentSection:toAbsoluteWire end rest

sectionEndPoint :: Point -> RelativeSection -> Point
sectionEndPoint start@(Point x y) (LEFT, distance) = Point (x - distance) y
sectionEndPoint start@(Point x y) (RIGHT, distance) = Point (x + distance) y
sectionEndPoint start@(Point x y) (UP, distance) = Point x (y + distance)
sectionEndPoint start@(Point x y) (DOWN, distance) = Point x (y - distance)

wireIntersections :: AbsoluteWire -> AbsoluteWire -> [Point]
wireIntersections wire1 wire2 = mapMaybe (uncurry intersectionPoint) (allCombinations wire1 wire2)

allCombinations :: [a] -> [b] -> [(a,b)]
allCombinations list1 list2 = [(x, y) | x <- list1, y <- list2]


minimizer :: Ord b => (a -> b) -> [a] -> Maybe a
minimizer _ [] = Nothing
minimizer func (firstElement:remainingList) = (Just . fst) (foldl (minimizerFolder func) (firstElement, func firstElement) remainingList)

minimizerFolder :: Ord b => (a -> b) -> (a , b) -> a -> (a , b)
minimizerFolder func oldTuple@(oldItem1, oldValue) newItem =
    let newValue = func newItem
    in if newValue < oldValue
        then (newItem, newValue)
        else oldTuple


minimalIntersection :: Point -> [RelativeWire] -> Maybe Point
minimalIntersection _ [] = Nothing
minimalIntersection _ [x] = Nothing
minimalIntersection start (x:(y:_)) =
    minimizer manhattenNorm intersectionPoints
    where intersectionPoints = filter (not . (==) start) (wireIntersections (toAbsoluteWire start x) (toAbsoluteWire start y))



