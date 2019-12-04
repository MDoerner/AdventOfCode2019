module AdventOfCode20191201_2
    ( advancedFuelCaculation
    ) where
        
import System.IO

advancedFuelCaculation :: IO ()
advancedFuelCaculation = do
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
requiredFuel 0 = 0
requiredFuel mass = fuelMass + requiredFuel fuelMass where
    fuelMass = requiredBaseFuel mass

requiredBaseFuel :: Int -> Int
requiredBaseFuel mass = max 0 ((quot mass 3) - 2)