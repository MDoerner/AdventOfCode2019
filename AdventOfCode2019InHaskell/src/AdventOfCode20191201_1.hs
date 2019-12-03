module AdventOfCode20191201_1
    ( simpleFuelCaculation
    ) where

import System.IO
import System.Directory
import System.FilePath

simpleFuelCaculation :: IO ()
simpleFuelCaculation = putStrLn "test" 

currentMassList :: IO [Int]
currentMassList = massList . currentFileName

currentFileName :: IO FilePath
currentFileName = Directory :: getCurrentWorkingDirectory

massList :: FilePath -> IO [Int] 
massList = [12]

requiredFuel :: Int -> Int
requiredFuel mass = max 0 ((quot mass 3) - 2)