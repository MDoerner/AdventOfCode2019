module AdventOfCode20191202_1
    ( gravityAssist
    ) where
        
import System.IO
import Data.List.Split
import Data.Maybe


gravityAssist :: IO ()
gravityAssist = do
    inputText <- readFile "Advent20191202_error_input.txt" 
    code <- return (toIntCode inputText)
    resultState <- return (executeCode code)
    putStrLn (show (startingElement resultState))

startingElement :: ProcessState -> Int
startingElement (ProcessState code _) = code!!0



type IntCode = [Int]

toIntCode :: String -> IntCode
toIntCode = (map read) . (splitOn ",")


data OpCode = Add | Multiply | Stop deriving (Eq, Show)

toOpCode :: Int -> Maybe OpCode
toOpCode 1 = Just Add
toOpCode 2 = Just Multiply
toOpCode 99 = Just Stop
toOpCode _ = Nothing

associatedOperation :: OpCode -> (ProcessState -> [Int] -> ProcessState)
associatedOperation Add = add
associatedOperation Multiply = multiply
associatedOperation Stop = stop

add :: ProcessState -> [Int] -> ProcessState
add = applyBinaryOperationAndWrite (+)

multiply :: ProcessState -> [Int] -> ProcessState
multiply = applyBinaryOperationAndWrite (*)

applyBinaryOperationAndWrite :: (Int -> Int -> Int) -> ProcessState -> [Int] -> ProcessState
applyBinaryOperationAndWrite _ state@(ProcessState _ Nothing) _ = state 
applyBinaryOperationAndWrite binaryOp (ProcessState code (Just pointer)) arguments = ProcessState newCode (Just (pointer + 4)) 
    where newCode = replaceAtIndex code (arguments!!2) (binaryOp (arguments!!0) (arguments!!1))

replaceAtIndex :: IntCode -> Int -> Int -> IntCode
replaceAtIndex code pointer value = 
    let (before, _:after) = splitAt pointer code
    in before ++ value:after

stop :: ProcessState -> [Int] -> ProcessState
stop (ProcessState code _) _ = ProcessState code Nothing 


data ArgumentMode = Pointer | Value deriving (Eq, Show)

toArgumentMode :: Int -> Maybe ArgumentMode
toArgumentMode 0 = Just Pointer
toArgumentMode 1 = Just Value
toArgumentMode _ = Nothing


data ProcessState = ProcessState {     
    code :: IntCode,    
    instructionPointer :: Maybe Int
    } deriving (Eq, Show)

hasShutDown :: ProcessState -> Bool
hasShutDown = isNothing . instructionPointer


data IntCodeInstruction = IntCodeInstruction {
    opcode :: OpCode,
    argumentModes :: [ArgumentMode]
}


executeCode :: IntCode -> ProcessState
executeCode code = continueExecution (ProcessState code (Just 0)) 

continueExecution :: ProcessState -> ProcessState
continueExecution state@(ProcessState code Nothing) = state
continueExecution state = 
    case intCodeInstruction state of 
        Nothing -> state
        Just instruction -> continueExecution (executeInstruction state instruction)

intCodeInstruction :: ProcessState -> Maybe IntCodeInstruction
intCodeInstruction (ProcessState _ Nothing) = Nothing
intCodeInstruction (ProcessState code (Just pointer)) = 
    case potentialOpcode of 
        Nothing -> Nothing
        Just opcode -> Just (IntCodeInstruction opcode [Pointer, Pointer, Value])
    where potentialOpcode = toOpCode (code!!pointer)

executeInstruction :: ProcessState -> IntCodeInstruction -> ProcessState
executeInstruction state@(ProcessState _ Nothing) _ = state
executeInstruction state instruction@(IntCodeInstruction opcode argumentModes) = 
    (associatedOperation opcode) state arguments
    where arguments = instructionArguments state instruction

instructionArguments :: ProcessState -> IntCodeInstruction -> [Int]
instructionArguments (ProcessState code Nothing) _ = []
instructionArguments (ProcessState code (Just pointer)) (IntCodeInstruction _ argumentModes) = 
    map (instructionArgument code pointer) numberedArgumentModes
    where numberedArgumentModes = zip argumentModes [1..]

instructionArgument :: IntCode -> Int -> (ArgumentMode, Int) -> Int
instructionArgument code instructionPointer (mode, offset) = case mode of
    Value -> code!!(instructionPointer + offset)
    Pointer -> code!!(code!!(instructionPointer + offset))
