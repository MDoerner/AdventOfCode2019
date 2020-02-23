module AdventOfCode20191205_2
    ( system5Diagnostic
    ) where

import System.IO
import Data.List.Split
import Data.Maybe


system5Diagnostic :: IO ()
system5Diagnostic = do
    inputText <- readFile "Advent20191205_1_input.txt"
    let code = toIntCode inputText
    let outputState = executeCode code [5]
    let output = outputs outputState
    print output



type IntCode = [Int]

toIntCode :: String -> IntCode
toIntCode = map read . splitOn ","


data OpCode = Add | Multiply | Get | Put | JumpIfTrue | JumpIfFalse | LessThan | Equals | Stop deriving (Eq, Show)

toOpCode :: Int -> Maybe OpCode
toOpCode 1 = Just Add
toOpCode 2 = Just Multiply
toOpCode 3 = Just Get
toOpCode 4 = Just Put
toOpCode 5 = Just JumpIfTrue
toOpCode 6 = Just JumpIfFalse
toOpCode 7 = Just LessThan
toOpCode 8 = Just Equals
toOpCode 99 = Just Stop
toOpCode _ = Nothing


--The returned Bool specifies whether the operation blocked.
associatedOperation :: OpCode -> (ProcessState -> [Int] -> (ProcessState, Bool))
associatedOperation Add = add
associatedOperation Multiply = multiply
associatedOperation Get = get
associatedOperation Put = put
associatedOperation JumpIfTrue = jumpIfTrue
associatedOperation JumpIfFalse = jumpIfFalse
associatedOperation LessThan = lessThan
associatedOperation Equals = equals
associatedOperation Stop = stop

--operation modes in order of arguments 
associatedOperationModes :: OpCode -> [OperationMode]
associatedOperationModes Add = [Read, Read, Write]
associatedOperationModes Multiply = [Read, Read, Write]
associatedOperationModes Get = [Write]
associatedOperationModes Put = [Read]
associatedOperationModes JumpIfTrue = [Read, Read]
associatedOperationModes JumpIfFalse = [Read, Read]
associatedOperationModes LessThan = [Read, Read, Write]
associatedOperationModes Equals = [Read, Read, Write]
associatedOperationModes Stop = []



add :: ProcessState -> [Int] -> (ProcessState, Bool)
add = applyBinaryOperationAndWrite (+)

multiply :: ProcessState -> [Int] -> (ProcessState, Bool)
multiply = applyBinaryOperationAndWrite (*)

applyBinaryOperationAndWrite :: (Int -> Int -> Int) -> ProcessState -> [Int] -> (ProcessState, Bool)
applyBinaryOperationAndWrite _ state@(ProcessState _ _ _ Nothing) _ = (state, True)
applyBinaryOperationAndWrite binaryOp (ProcessState memoryState inputs outputs (Just instructionPointer)) arguments =
    (ProcessState newMemoryState inputs outputs newInstructionPointer, False)
    where
        newMemoryState = writeToMemory memoryState (arguments!!2) (binaryOp (head arguments) (arguments!!1))
        newInstructionPointer = Just (instructionPointer + 4)

get :: ProcessState -> [Int] -> (ProcessState, Bool)
get state@(ProcessState _ _ _ Nothing) _ = (state, True)
get state@(ProcessState _ [] _ _) _ = (state, True) -- block since there is no input

get (ProcessState memoryState (firstInput:remainingInputs) outputs (Just instructionPointer)) arguments =
    (ProcessState newMemoryState remainingInputs outputs newInstructionPointer, False)
    where
        newMemoryState = writeToMemory memoryState (head arguments) firstInput
        newInstructionPointer = Just (instructionPointer + 2)

put :: ProcessState -> [Int] -> (ProcessState, Bool)
put state@(ProcessState _ _ _ Nothing) _ = (state, True)
put (ProcessState memoryState inputs outputs (Just instructionPointer)) arguments =
    (ProcessState memoryState inputs newOutputs newInstructionPointer, False)
    where
        newOutputs = let newOutputValue = (head arguments)
                        in outputs ++ [newOutputValue]
        newInstructionPointer = Just (instructionPointer + 2)

jumpIfTrue :: ProcessState -> [Int] -> (ProcessState, Bool)
jumpIfTrue = jumpIf (/= 0)

jumpIfFalse :: ProcessState -> [Int] -> (ProcessState, Bool)
jumpIfFalse = jumpIf (== 0)

jumpIf :: (Int -> Bool) -> ProcessState -> [Int] -> (ProcessState, Bool)
jumpIf _ state@(ProcessState _ _ _ Nothing) _ = (state, True)
jumpIf test state@(ProcessState memoryState inputs outputs (Just instructionPointer)) arguments
    | test (head arguments) = let newInstructionPointer = Just (arguments!!1)
                                in  (ProcessState memoryState inputs outputs newInstructionPointer, False)
    | otherwise = let newInstructionPointer = Just (instructionPointer + 3)
                    in  (ProcessState memoryState inputs outputs newInstructionPointer, False)

lessThan :: ProcessState -> [Int] -> (ProcessState, Bool)
lessThan = applyBinaryComparisonAndWrite (<)

equals :: ProcessState -> [Int] -> (ProcessState, Bool)
equals = applyBinaryComparisonAndWrite (==)

applyBinaryComparisonAndWrite :: (Int -> Int -> Bool) -> ProcessState -> [Int] -> (ProcessState, Bool)
applyBinaryComparisonAndWrite _ state@(ProcessState _ _ _ Nothing) _ = (state, True)
applyBinaryComparisonAndWrite binaryComp (ProcessState memoryState inputs outputs (Just instructionPointer)) arguments =
    (ProcessState newMemoryState inputs outputs newInstructionPointer, False)
    where
        newMemoryState = writeToMemory memoryState (arguments!!2) (if head arguments `binaryComp` (arguments!!1) then 1 else 0)
        newInstructionPointer = Just (instructionPointer + 4)

stop :: ProcessState -> [Int] -> (ProcessState, Bool)
stop (ProcessState memoryState inputs outputs _) _ = (ProcessState memoryState inputs outputs Nothing, True)


data ArgumentSpecification = ArgumentSpecification {
    argumentMode :: ArgumentMode,
    operationMode :: OperationMode
} deriving (Eq, Show)

data OperationMode = Read | Write deriving (Eq, Show)
data ArgumentMode = Pointer | Value deriving (Eq, Show)

toArgumentMode :: Int -> Maybe ArgumentMode
toArgumentMode 0 = Just Pointer
toArgumentMode 1 = Just Value
toArgumentMode _ = Nothing


newtype MemoryState = MemoryState IntCode deriving (Eq, Show)

readMemory :: MemoryState -> Int -> Int
readMemory (MemoryState internalMemory) pointer = internalMemory!!pointer

writeToMemory :: MemoryState -> Int -> Int -> MemoryState
writeToMemory (MemoryState internalMemory) pointer value = MemoryState (replaceAtIndex internalMemory pointer value)

replaceAtIndex :: IntCode -> Int -> Int -> IntCode
replaceAtIndex code pointer value =
    let (before, _:after) = splitAt pointer code
    in before ++ value:after


data ProcessState = ProcessState {
    memory :: MemoryState,
    inputs :: InputStream,
    outputs :: [Int],
    instructionPointer :: Maybe Int
} deriving (Eq, Show)

hasShutDown :: ProcessState -> Bool
hasShutDown = isNothing . instructionPointer

type InputStream = [Int]


data IntCodeInstruction = IntCodeInstruction {
    opcode :: OpCode,
    argumentSpecifications :: [ArgumentSpecification]
} deriving (Eq, Show)


executeCode :: IntCode -> InputStream -> ProcessState
executeCode code inputs = continueExecution (ProcessState (MemoryState code) inputs [] (Just 0))

continueExecution :: ProcessState -> ProcessState
continueExecution state@(ProcessState _ _ _ Nothing) = state
continueExecution state =
    case intCodeInstruction state of
        Nothing -> state
        Just instruction -> let (newState, operationBlocked) = executeInstruction state instruction
                            in if operationBlocked
                                then newState
                                else continueExecution newState

intCodeInstruction :: ProcessState -> Maybe IntCodeInstruction
intCodeInstruction (ProcessState _ _ _ Nothing) = Nothing
intCodeInstruction (ProcessState memory _ _ (Just instructionPointer)) =
    case potentialOpcode of
        Nothing -> Nothing
        Just opcode -> let potentialArgumentSpecifications = toArgumentSpecifications opcode (instruction `div` 100)
                        in case potentialArgumentSpecifications of
                            Nothing -> Nothing
                            Just argumentSpecifications -> Just (IntCodeInstruction opcode argumentSpecifications)
    where
        instruction = readMemory memory instructionPointer
        potentialOpcode = toOpCode (instruction `mod` 100)

toArgumentSpecifications :: OpCode -> Int -> Maybe [ArgumentSpecification]
toArgumentSpecifications opcode argumentSpecifier
    | isNothing specifiedArgumentModes = Nothing
    | any isNothing (fromJust specifiedArgumentModes) = Nothing
    | otherwise =
        let numberOfMissingElements = length operationModes - length (fromJust specifiedArgumentModes)
        in if numberOfMissingElements < 0
            then Nothing
            else let paddedArgumentsModes = catMaybes (fromJust specifiedArgumentModes) ++ replicate numberOfMissingElements Pointer
                in Just (zipWith ArgumentSpecification paddedArgumentsModes operationModes)
    where
        specifiedArgumentModes = argumentModesFromSpecifier argumentSpecifier
        operationModes = associatedOperationModes opcode

argumentModesFromSpecifier :: Int -> Maybe [Maybe ArgumentMode]
argumentModesFromSpecifier 0 = Just []
argumentModesFromSpecifier x
    | x < 0 = Nothing
    | otherwise = Just (map toArgumentMode (reverse (toDigits x)))

toDigits ::  Integral x => x -> [x]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

-- The Bool return value specifies whether the operation has blocked.
executeInstruction :: ProcessState -> IntCodeInstruction -> (ProcessState, Bool)
executeInstruction state@(ProcessState _ _ _ Nothing) _ = (state, True)
executeInstruction state instruction@(IntCodeInstruction opcode _) =
    associatedOperation opcode state arguments
    where arguments = instructionArguments state instruction

instructionArguments :: ProcessState -> IntCodeInstruction -> [Int]
instructionArguments (ProcessState _ _ _ Nothing) _ = []
instructionArguments (ProcessState memory _ _ (Just instructionPointer)) (IntCodeInstruction _ argumentSpecifications) =
    map (instructionArgument memory instructionPointer) numberedArgumentSpecifications
    where numberedArgumentSpecifications = zip argumentSpecifications [1..]

instructionArgument :: MemoryState -> Int -> (ArgumentSpecification, Int) -> Int
instructionArgument memory instructionPointer (argumentSpecification, offset) =
    case argumentMode argumentSpecification of
        Value -> immediateArgument
        Pointer -> case operationMode argumentSpecification of
                    Read -> readMemory memory immediateArgument
                    Write -> immediateArgument
    where immediateArgument = readMemory memory (instructionPointer + offset)

