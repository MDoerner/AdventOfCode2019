module AdventOfCode20191209_2
    (
        sensorBoost
    ) where

import System.IO
import Data.List.Split
import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import Control.Monad.State
import Data.Digits


sensorBoost :: IO ()
sensorBoost = do
    inputText <- readFile "Advent20191209_1_input.txt"
    let code = toIntCode inputText
    let result = executeCode code [2]
    print result


type IntCode = [Int]

toIntCode :: String -> IntCode
toIntCode = map read . splitOn ","




--Memory

newtype Memory = Memory (Map.HashMap Int Int) deriving (Eq, Show)

readMemory :: Int -> State Memory Int
readMemory pointer = gets $ \(Memory m) -> Map.lookupDefault 0 pointer m

writeToMemory :: Int -> Int -> State Memory ()
writeToMemory pointer 0 = modify $ \(Memory m) -> Memory $ Map.delete pointer m
writeToMemory pointer value = modify $ \(Memory m) -> Memory $ Map.alter (\_ -> Just value) pointer m

fromIntCode :: IntCode -> Memory
fromIntCode = Memory . Map.fromList . indexedIntCode

indexedIntCode :: IntCode -> [(Int, Int)]
indexedIntCode = zip [0..]



--InputStream

newtype InputStream = InputStream [Int] deriving (Eq, Show)

addInput :: Int -> State InputStream ()
addInput input = modify $ \(InputStream xs) -> InputStream $ xs ++ [input]

addInputs :: [Int] -> State InputStream ()
addInputs inputs = modify $ \(InputStream xs) -> InputStream $ xs ++ inputs

popInput :: State InputStream (Maybe Int)
popInput = state $ \input@(InputStream xs) -> case xs of
    [] -> (Nothing, input)
    y:ys -> (Just y, InputStream ys)



--ProcessState

data ExecutionStatus = Running | Blocked | Terminated | Error deriving (Eq, Show, Enum)

data ProcessState = ProcessState {
    memory :: Memory,
    inputs :: InputStream,
    instructionPointer :: Int,
    relativeBasePointer :: Int,
    status :: ExecutionStatus
} deriving (Eq, Show)

processStatus :: State ProcessState ExecutionStatus
processStatus = gets $ status

hasShutDown :: State ProcessState Bool
hasShutDown = do
    currentStatus <- processStatus
    case currentStatus of
        Terminated -> return True
        Error -> return True
        _ -> return False

isRunning :: State ProcessState Bool
isRunning = do
    currentStatus <- processStatus
    case currentStatus of
        Running -> return True
        _ -> return False

setProcessStatus :: ExecutionStatus -> State ProcessState ()
setProcessStatus processStatus = do
    stopped <- hasShutDown
    if stopped
        then return ()
        else modify $ \s -> s{status = processStatus}

terminateProcess :: State ProcessState ()
terminateProcess = setProcessStatus Terminated

abortProcess :: State ProcessState ()
abortProcess = setProcessStatus Error


setInstructionPointer :: Int -> State ProcessState ()
setInstructionPointer pointer = modify $ \s -> s {instructionPointer = pointer}

processInstructionPointer :: State ProcessState Int
processInstructionPointer = gets $ instructionPointer

incrementInstructionPointer :: Int -> State ProcessState ()
incrementInstructionPointer offset = do
    instructionPointer <- processInstructionPointer
    setInstructionPointer (instructionPointer + offset)


setRelativeBasePointer :: Int -> State ProcessState ()
setRelativeBasePointer pointer = modify $ \s -> s {relativeBasePointer = pointer}

processRelativeBasePointer :: State ProcessState Int
processRelativeBasePointer = gets $ relativeBasePointer

incrementRelativeBasePointer :: Int -> State ProcessState ()
incrementRelativeBasePointer offset = do
    relativeBasePointer <- processRelativeBasePointer
    setRelativeBasePointer (relativeBasePointer + offset)


readProcessMemory :: Int -> State ProcessState Int
readProcessMemory pointer = gets $ \ProcessState{memory = m} -> evalState (readMemory pointer) m

writeToProcessMemory :: Int -> Int -> State ProcessState ()
writeToProcessMemory pointer value = modify $ \s@ProcessState{memory = m} -> s {memory = execState (writeToMemory pointer value) m}


addProcessInput :: Int -> State ProcessState ()
addProcessInput additionalInput = modify $ \s@ProcessState{inputs = inputStream} -> s {inputs = execState (addInput additionalInput) inputStream}

addProcessInputs :: [Int] -> State ProcessState ()
addProcessInputs additionalInputs = modify $ \s@ProcessState{inputs = inputStream} -> s {inputs = execState (addInputs additionalInputs) inputStream}

popProcessInput :: State ProcessState (Maybe Int)
popProcessInput = state $ \s@ProcessState{inputs = inputStream} ->
    let (input, newInputs) = runState popInput inputStream
        in (input, s {inputs = newInputs})


initializeProcess :: IntCode -> [Int] -> ProcessState
initializeProcess code initialInputs = ProcessState {
    memory = fromIntCode code,
    inputs = InputStream initialInputs,
    instructionPointer = 0,
    relativeBasePointer = 0,
    status = Running}


--IntCodeInstruction

data IntCodeInstruction = IntCodeInstruction {
    opcode :: OpCode,
    argumentSpecifications :: [ArgumentSpecification]
} deriving (Eq, Show)


--ArgumentSpecification

data ArgumentSpecification = ArgumentSpecification {
    argumentMode :: ArgumentMode,
    operationMode :: OperationMode
} deriving (Eq, Show)

data OperationMode = Read | Write deriving (Eq, Show)
data ArgumentMode = Pointer | Value | Relative deriving (Eq, Show)

toArgumentMode :: Int -> Maybe ArgumentMode
toArgumentMode 0 = Just Pointer
toArgumentMode 1 = Just Value
toArgumentMode 2 = Just Relative
toArgumentMode _ = Nothing


--Operations

data OpCode = Add | Multiply | Get | Put | JumpIfTrue | JumpIfFalse | LessThan | Equals | IncrementRelativeBase | Stop deriving (Eq, Show, Enum)

toOpCode :: Int -> Maybe OpCode
toOpCode 1 = Just Add
toOpCode 2 = Just Multiply
toOpCode 3 = Just Get
toOpCode 4 = Just Put
toOpCode 5 = Just JumpIfTrue
toOpCode 6 = Just JumpIfFalse
toOpCode 7 = Just LessThan
toOpCode 8 = Just Equals
toOpCode 9 = Just IncrementRelativeBase
toOpCode 99 = Just Stop
toOpCode _ = Nothing

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
associatedOperationModes IncrementRelativeBase = [Read]
associatedOperationModes Stop = []

type Arguments = [Int]

associatedOperation :: OpCode -> (Arguments -> State ProcessState (Maybe Int))
associatedOperation Add = handleTerminationAndRun . add
associatedOperation Multiply = handleTerminationAndRun . multiply
associatedOperation Get = handleTerminationAndRun . getOperation
associatedOperation Put = handleTerminationAndRun . putOperation
associatedOperation JumpIfTrue = handleTerminationAndRun . jumpIfTrue
associatedOperation JumpIfFalse = handleTerminationAndRun . jumpIfFalse
associatedOperation LessThan = handleTerminationAndRun . lessThan
associatedOperation Equals = handleTerminationAndRun . equals
associatedOperation IncrementRelativeBase = handleTerminationAndRun . incrementRelativeBase
associatedOperation Stop = handleTerminationAndRun . stop

handleTerminationAndRun :: State ProcessState (Maybe Int) -> State ProcessState (Maybe Int)
handleTerminationAndRun state = do
    stopped <- hasShutDown
    if stopped
        then return Nothing
        else state


add :: Arguments -> State ProcessState (Maybe Int)
add = applyBinaryOperationAndWrite (+)

multiply :: Arguments -> State ProcessState (Maybe Int)
multiply = applyBinaryOperationAndWrite (*)

applyBinaryOperationAndWrite :: (Int -> Int -> Int) -> (Arguments -> State ProcessState (Maybe Int))
applyBinaryOperationAndWrite binaryOp arguments = do
    let
        targetPointer = arguments!!2
        value = binaryOp (head arguments) (arguments!!1)
        in writeToProcessMemory targetPointer value
    incrementInstructionPointer 4
    setProcessStatus Running
    return Nothing

getOperation :: Arguments -> State ProcessState (Maybe Int)
getOperation arguments = do
    maybeInput <- popProcessInput
    case maybeInput of
        Nothing -> do
            setProcessStatus Blocked
            return Nothing
        Just input -> do
            let
                targetPointer = head arguments
                in writeToProcessMemory targetPointer input
            incrementInstructionPointer 2
            setProcessStatus Running
            return Nothing


putOperation :: Arguments -> State ProcessState (Maybe Int)
putOperation arguments = do
    incrementInstructionPointer 2
    setProcessStatus Running
    let newOutputValue = head arguments
        in return $ Just newOutputValue


jumpIfTrue :: Arguments -> State ProcessState (Maybe Int)
jumpIfTrue = jumpIf (/= 0)

jumpIfFalse :: Arguments -> State ProcessState (Maybe Int)
jumpIfFalse = jumpIf (== 0)

jumpIf :: (Int -> Bool) -> (Arguments -> State ProcessState (Maybe Int))
jumpIf test arguments = do
    if test (head arguments)
        then setInstructionPointer (arguments!!1)
        else incrementInstructionPointer 3
    setProcessStatus Running
    return Nothing


lessThan :: Arguments -> State ProcessState (Maybe Int)
lessThan = applyBinaryComparisonAndWrite (<)

equals :: Arguments -> State ProcessState (Maybe Int)
equals = applyBinaryComparisonAndWrite (==)

applyBinaryComparisonAndWrite :: (Int -> Int -> Bool) -> (Arguments -> State ProcessState (Maybe Int))
applyBinaryComparisonAndWrite binaryComp arguments = do
    let
        targetPointer = arguments!!2
        value = if (head arguments) `binaryComp` (arguments!!1)
            then 1
            else 0
        in writeToProcessMemory targetPointer value
    incrementInstructionPointer 4
    setProcessStatus Running
    return Nothing


incrementRelativeBase :: Arguments -> State ProcessState (Maybe Int)
incrementRelativeBase arguments = do
    incrementRelativeBasePointer $ head arguments
    incrementInstructionPointer 2
    setProcessStatus Running
    return Nothing


stop :: Arguments -> State ProcessState (Maybe Int)
stop _ = do
    terminateProcess
    return Nothing



--Code Execution

executeCode :: IntCode -> [Int] -> [Int]
executeCode code initialInputs =
    let initialState = initializeProcess code initialInputs
        in evalState continueExecution initialState

continueExecution :: State ProcessState [Int]
continueExecution = do
    maybeResult <- executeNextInstruction
    running <- isRunning
    if running
        then do
            remainingResult <- continueExecution
            case maybeResult of
                Nothing -> return remainingResult
                Just result -> return (result:remainingResult)
        else return []

executeNextInstruction :: State ProcessState (Maybe Int)
executeNextInstruction = do
    maybeInstruction <- intCodeInstruction
    case maybeInstruction of
        Nothing -> do
            abortProcess
            return Nothing
        Just instruction -> executeInstruction instruction

executeInstruction :: IntCodeInstruction -> State ProcessState (Maybe Int)
executeInstruction instruction = do
    arguments <- instructionArguments instruction
    let operation = associatedOperation (opcode instruction)
        in operation arguments


intCodeInstruction :: State ProcessState (Maybe IntCodeInstruction)
intCodeInstruction = do
    instructionPointer <- processInstructionPointer
    instructionValue <- readProcessMemory instructionPointer
    return (do -- Maybe
        opcode <- toOpCode (instructionValue `mod` 100)
        argumentSpecs <- toArgumentSpecifications opcode (instructionValue `div` 100)
        return (IntCodeInstruction opcode argumentSpecs))

toArgumentSpecifications :: OpCode -> Int -> Maybe [ArgumentSpecification]
toArgumentSpecifications opcode argumentSpecifier =
    do -- Maybe
        maybeSpecifiedArgumentModes <- argumentModesFromSpecifier argumentSpecifier
        specifiedArgumentModes <- sequence maybeSpecifiedArgumentModes
        let
            operationModes = associatedOperationModes opcode
            numberOfMissingElements = length operationModes - length specifiedArgumentModes
            in if numberOfMissingElements < 0
                then Nothing
                else let paddedArgumentsModes = specifiedArgumentModes ++ replicate numberOfMissingElements Pointer
                        in return (zipWith ArgumentSpecification paddedArgumentsModes operationModes)

argumentModesFromSpecifier :: Int -> Maybe [Maybe ArgumentMode]
argumentModesFromSpecifier 0 = Just []
argumentModesFromSpecifier x
    | x < 0 = Nothing
    | otherwise = Just (map toArgumentMode (reverse (digits 10 x)))

instructionArguments :: IntCodeInstruction -> State ProcessState Arguments
instructionArguments instruction = do
    basePointer <- processInstructionPointer
    let enumeratedArgumentSpecifications = zip [1..] (argumentSpecifications instruction)
        in mapM (instructionArgument basePointer) enumeratedArgumentSpecifications

instructionArgument :: Int -> (Int, ArgumentSpecification) -> State ProcessState Int
instructionArgument basePointer (offset, argumentSpec) =
    let evaluationPointer = basePointer + offset
    in case argumentMode argumentSpec of
        Value -> readProcessMemory evaluationPointer
        Pointer -> case operationMode argumentSpec of
                    Write -> readProcessMemory evaluationPointer
                    Read -> do
                        transitiveEvaluationPointer <- readProcessMemory evaluationPointer
                        readProcessMemory transitiveEvaluationPointer
        Relative -> do
            relativeBase <- processRelativeBasePointer
            baseIncrement <- readProcessMemory evaluationPointer
            let targetPointer = relativeBase + baseIncrement
                in case operationMode argumentSpec of
                    Write -> return targetPointer
                    Read -> readProcessMemory targetPointer