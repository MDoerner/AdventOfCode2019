module AdventOfCode20191201_1
    ( simpleFuelCaculation
    ) where
        
import System.IO

simpleFuelCaculation :: IO ()
simpleFuelCaculation = do
    inputText <- readFile "Advent20191201_1_input.txt" 
    requiredFuel <- return (totalRequiredFuel inputText)
    putStrLn (show requiredFuel)

totalRequiredFuel :: String -> Int
totalRequiredFuel = sum . fuelList . massList

massList :: String -> [Int]
massList = (map read) . lines

fuelList :: [Int] -> [Int]
fuelList = map requiredFuel

requiredFuel :: Int -> Int
requiredFuel mass = max 0 ((quot mass 3) - 2)