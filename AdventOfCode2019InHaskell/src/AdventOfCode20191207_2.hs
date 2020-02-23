module AdventOfCode20191207_2
    (
        loopedAmplifierOptimization,
        bestLoopedAmplifierSettings
    ) where

import System.IO
import Data.List.Split
import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import Control.Monad.State
import Data.Digits
import Data.Traversable


loopedAmplifierOptimization :: IO ()
loopedAmplifierOptimization = do
    inputText <- readFile "Advent20191207_1_input.txt"
    let code = toIntCode inputText
    let bestBoost = highestBoost sum [0] code [5..9]
    print bestBoost

bestLoopedAmplifierSettings :: IO ()
bestLoopedAmplifierSettings = do
    inputText <- readFile "Advent20191207_1_input.txt"
    let code = toIntCode inputText
    let bestSettings = bestPhaseSetting sum [0] code [5..9]
    print bestSettings


type IntCode = [Int]

toIntCode :: String -> IntCode
toIntCode = map read . splitOn ","



-- Amplifier Optimization

type PhaseSetting = Int

bestPhaseSetting :: Ord a => ([Int] -> a) -> [Int] -> IntCode -> [PhaseSetting] -> [PhaseSetting]
bestPhaseSetting evalFunction input code phaseSettings =
    maximumBy (\x y -> compare (phaseSettingScore evalFunction input code x) (phaseSettingScore evalFunction input code y)) (permutations phaseSettings)

phaseSettingScore :: Ord a => ([Int] -> a) -> [Int] -> IntCode -> [PhaseSetting] -> a
phaseSettingScore evalFunction input code = evalFunction . (executeAmplifier input code)

highestBoost :: Ord a => ([Int] -> a) -> [Int] -> IntCode -> [PhaseSetting] -> a
highestBoost evalFunction input code = maximum . (map evalFunction) . (possibleExecutionResults input code)

possibleExecutionResults :: [Int] -> IntCode -> [PhaseSetting] -> [[Int]]
possibleExecutionResults input code phaseSettings = map (executeAmplifier input code) (permutations phaseSettings)

executeAmplifier :: [Int] -> IntCode -> [PhaseSetting] -> [Int]
executeAmplifier input code phaseSettings = executeProcessArrayLooped input (amplifierConfiguration code phaseSettings)

possibleAmplifierConfiguration :: IntCode -> [PhaseSetting] -> [ProcessArrayConfiguration]
possibleAmplifierConfiguration code phaseSettings = map (amplifierConfiguration code) (permutations phaseSettings)

amplifierConfiguration :: IntCode -> [PhaseSetting] -> ProcessArrayConfiguration
amplifierConfiguration code phaseSettings = zip (repeat code) (map pure phaseSettings)



-- ProcessArray

type ProcessArray = [ProcessState]
type ProcessArrayConfiguration = [(IntCode, [Int])]

initializeProcessArray :: ProcessArrayConfiguration -> ProcessArray
initializeProcessArray = map (uncurry initializeProcess)

executeProcessArrayOnce :: [Int] -> ProcessArrayConfiguration -> [Int]
executeProcessArrayOnce input configuration = evalState (addInputsAndRunProcessArrayOnce input) (initializeProcessArray configuration)

addInputsAndRunProcessArrayOnce :: [Int] -> State ProcessArray [Int]
addInputsAndRunProcessArrayOnce input = state $ \processArray -> mapAccumL (\x -> runState (addInputsAndRunProcess x)) input processArray

addInputsAndRunProcess :: [Int] -> State ProcessState [Int]
addInputsAndRunProcess input = do
    addProcessInputs input
    continueExecution


executeProcessArrayLooped :: [Int] -> ProcessArrayConfiguration -> [Int]
executeProcessArrayLooped input configuration = evalState (addInputsAndRunProcessArrayLooped input) (initializeProcessArray configuration)

addInputsAndRunProcessArrayLooped :: [Int] -> State ProcessArray [Int]
addInputsAndRunProcessArrayLooped input = do
    loopOutput <- addInputsAndRunProcessArrayOnce input
    processes <- get
    let finalProcessor = last processes
    let hasStopped = evalState hasShutDown finalProcessor
    if hasStopped
        then return loopOutput
        else addInputsAndRunProcessArrayLooped loopOutput



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
initializeProcess code initialInputs = ProcessState { memory = fromIntCode code, inputs = InputStream initialInputs, instructionPointer = 0, status = Running}


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
data ArgumentMode = Pointer | Value deriving (Eq, Show)

toArgumentMode :: Int -> Maybe ArgumentMode
toArgumentMode 0 = Just Pointer
toArgumentMode 1 = Just Value
toArgumentMode _ = Nothing


--Operations

data OpCode = Add | Multiply | Get | Put | JumpIfTrue | JumpIfFalse | LessThan | Equals | Stop deriving (Eq, Show, Enum)

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

executeInstruction :: IntCodeInstruction -> State ProcessState (Maybe Int)
executeInstruction instruction = do
    arguments <- instructionArguments instruction
    let operation = associatedOperation (opcode instruction)
        in operation arguments

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