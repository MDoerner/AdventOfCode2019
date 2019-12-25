from __future__ import annotations
from enum import Enum, IntEnum, auto
from typing import List, Dict, Tuple, Any, IO, Optional
import queue

IntCode = List[int]

class OpCode(IntEnum):
    ADD = 1
    MULTIPLY = 2
    GET = 3
    PUT = 4
    JUMP_IF_TRUE = 5
    JUMP_IF_FALSE = 6
    LESS_THEN = 7
    EQUALS = 8
    SETBASE = 9
    STOP = 99


class ArgumentMode(IntEnum):
    POINTER = 0
    VALUE = 1
    RELATIVE = 2


class OperationMode(Enum):
    READ = auto()
    WRITE = auto()


class IntCodeInstruction:
    _operation_modes : Dict[OpCode, List[OperationMode]] = {
        OpCode.ADD: [OperationMode.READ, OperationMode.READ, OperationMode.WRITE],
        OpCode.MULTIPLY: [OperationMode.READ, OperationMode.READ, OperationMode.WRITE],
        OpCode.GET: [OperationMode.WRITE],
        OpCode.PUT: [OperationMode.READ],
        OpCode.JUMP_IF_TRUE: [OperationMode.READ, OperationMode.READ],
        OpCode.JUMP_IF_FALSE: [OperationMode.READ, OperationMode.READ],
        OpCode.LESS_THEN: [OperationMode.READ, OperationMode.READ, OperationMode.WRITE],
        OpCode.EQUALS: [OperationMode.READ, OperationMode.READ, OperationMode.WRITE],
        OpCode.SETBASE: [OperationMode.READ],
        OpCode.STOP: []
    }

    def __init__(self, instruction : int):
        self.opcode = self._op_code(instruction)
        self.argument_specifications = self._argument_specifications(instruction // 100, self.opcode)

    def number_of_arguments(self) -> int:
        return len(self.argument_specifications)

    def _op_code(self, instruction : int) -> OpCode:
        return instruction % 100

    def _argument_specifications(self, argument_mode_specification : int, opcode : OpCode) -> List[Tuple[ArgumentMode, OperationMode]]:
        return zip(self._argument_modes(argument_mode_specification, self.opcode), self._operation_modes[self.opcode])

    def _argument_modes(self, argument_mode_specification : int, opcode : OpCode) -> List[ArgumentMode]:
        number_of_arguments = len(self._operation_modes[opcode])
        if number_of_arguments == 0:
            return []
        modes = self._visible_argument_modes(argument_mode_specification)
        self._fill_modes_for_omitted_specification(modes, number_of_arguments)
        return modes

    def _visible_argument_modes(self, argument_mode_specification : int) -> List[ArgumentMode]:
        modes = self._to_digits(argument_mode_specification)
        return list(reversed(modes))

    def _to_digits(self, natural_number : int) ->  List[int]:
        return [int(d) for d in str(natural_number)]

    def _fill_modes_for_omitted_specification(self, modes : List[ArgumentMode], number_of_arguements : int):
        number_of_missing_arguments = number_of_arguements - len(modes)
        if number_of_missing_arguments > 0:
            missing_modes = [0 for x in range(number_of_missing_arguments)]
            modes.extend(missing_modes)


class IntCodeReader:
    def int_code(self, filename : str) -> IO[IntCode]:
        input_string = self._read_input(filename)
        return self.to_int_code(input_string)

    def _read_input(self, filename : str) -> IO[str]:
        with open(filename, "r") as file:
            contents = file.read()
        return contents

    def to_int_code(self, input_string : str) -> IntCode:
        return [int(x) for x in input_string.split(',')]


class Memory:
    def __init__(self, code : IntCode):
        self._code = code
        self._internal_memory_size = len(self._code)
        self._external_memory : Dict[int, int] = {}

    def read(self, pointer : int) -> int:
        if 0 <= pointer < self._internal_memory_size:
            return self._code[pointer]
        return self._external_memory.get(pointer, 0)

    def write(self, pointer : int, value : int):
        if 0 <= pointer < self._internal_memory_size:
            self._code[pointer] = value
        else:
            if value != 0:
                self._external_memory[pointer] = value
            else:
                if pointer in self._external_memory:
                    del self._external_memory[pointer]


class ProcessState:
    def __init__(self, memory : Memory, instruction_pointer : int, base_pointer : int, input_stream : List[int], output : List[int], output_since_blocking_last : List[int]):
        self.memory = memory
        self.instruction_pointer = instruction_pointer
        self.base_pointer = base_pointer
        self.input_stream = input_stream
        self.output = output
        self.output_since_blocking_last = output_since_blocking_last

    def has_shut_down(self) -> bool:
        return self.instruction_pointer is None

    def add_input(self, input_list : List[int]):
        self.input_stream = [item for item in reversed(input_list)] + self.input_stream

    def add_output(self, output_list : List[int]):
        self.output.extend(output_list)
        self.output_since_blocking_last.extend(output_list)

    def clear_output_since_blocking_last(self):
        self.output_since_blocking_last.clear()


class BlockingIntCodeProcessor:
    def execute_code(self, code : IntCode, input_stream : List[int]) -> ProcessState:
        memory = Memory(code)
        process_state = ProcessState(memory, 0, 0, input_stream, [], [])
        self.continue_execution(process_state)
        return process_state

    def continue_execution(self, process_state : ProcessState):
        process_state.clear_output_since_blocking_last()
        instruction_blocks = False
        while not instruction_blocks and not process_state.has_shut_down():
            current_instruction = self._invocation_data(process_state)
            instruction_blocks = self._execute_operation(process_state, current_instruction)

    def _invocation_data(self, process_state : ProcessState) -> IntCodeInstruction:
        instruction = process_state.memory.read(process_state.instruction_pointer)
        return IntCodeInstruction(instruction)

    def _execute_operation(self, process_state : ProcessState, instruction : IntCodeInstruction) -> bool:
        opcode = instruction.opcode
        arguments = self._operation_arguments(process_state, instruction)
        if opcode == OpCode.STOP:
            return self._stop(process_state)
        if opcode == OpCode.ADD:
            return self._add(process_state, arguments[0], arguments[1], arguments[2])
        elif opcode == OpCode.MULTIPLY:
            return self._multiply(process_state, arguments[0], arguments[1], arguments[2])
        elif opcode == OpCode.GET:
            return self._get(process_state, arguments[0])
        elif opcode == OpCode.PUT:
            return self._put(process_state, arguments[0])
        elif opcode == OpCode.JUMP_IF_TRUE:
            return self._jump_if_true(process_state, arguments[0], arguments[1])
        elif opcode == OpCode.JUMP_IF_FALSE:
            return self._jump_if_false(process_state, arguments[0], arguments[1])
        elif opcode == OpCode.LESS_THEN:
            return self._less_than(process_state, arguments[0], arguments[1], arguments[2])
        elif opcode == OpCode.EQUALS:
            return self._equal(process_state, arguments[0], arguments[1], arguments[2])
        elif opcode == OpCode.SETBASE:
            return self._set_base_pointer(process_state, arguments[0])
        return None

    def _operation_arguments(self, process_state : ProcessState, instruction : IntCodeInstruction) -> List[int]:
        return [self._operation_argument(process_state, argument_index + 1, argument_specification) \
            for (argument_index, argument_specification) in enumerate(instruction.argument_specifications)]

    def _operation_argument(self, process_state : ProcessState, argument_offset : int, argument_specification : Tuple[ArgumentMode, OperationMode]) -> int:
        (argument_mode, operation_mode) = argument_specification
        argument_pointer = process_state.instruction_pointer + argument_offset
        memory = process_state.memory
        immediate_value = memory.read(argument_pointer)
        if argument_mode == ArgumentMode.VALUE or argument_mode == ArgumentMode.POINTER and operation_mode == OperationMode.WRITE:
            return immediate_value
        if argument_mode == ArgumentMode.POINTER and operation_mode == OperationMode.READ:
            return memory.read(immediate_value)
        if argument_mode == ArgumentMode.RELATIVE:
            target_pointer = process_state.base_pointer + immediate_value
            if operation_mode == OperationMode.WRITE:
                return target_pointer
            return memory.read(target_pointer)
        return None

    def _stop(self, process_state : ProcessState) -> bool:
        process_state.instruction_pointer = None
        return False

    def _add(self, process_state : ProcessState, arg1 : int, arg2 : int, target_index : int) -> bool:
        process_state.memory.write(target_index, arg1 + arg2)
        process_state.instruction_pointer = process_state.instruction_pointer + 4
        return False

    def _multiply(self, process_state : ProcessState, arg1 : int, arg2 : int, target_index : int) -> bool:
        process_state.memory.write(target_index, arg1 * arg2)
        process_state.instruction_pointer = process_state.instruction_pointer + 4
        return False

    def _get(self, process_state : ProcessState, target_index : int) -> bool:
        if not process_state.input_stream:
            return True
        value = process_state.input_stream.pop()
        process_state.memory.write(target_index, value)
        process_state.instruction_pointer = process_state.instruction_pointer + 2
        return False

    def _put(self, process_state : ProcessState, value : int) -> bool:
        process_state.add_output([value])
        process_state.instruction_pointer = process_state.instruction_pointer + 2
        return False

    def _jump_if_true(self, process_state : ProcessState, value, target_index) -> bool:
        if value != 0:
            process_state.instruction_pointer = target_index
        else:
            process_state.instruction_pointer = process_state.instruction_pointer + 3
        return False

    def _jump_if_false(self, process_state : ProcessState, value : int, target_index : int) -> bool:
        if value == 0:
            process_state.instruction_pointer = target_index
        else:
            process_state.instruction_pointer = process_state.instruction_pointer + 3
        return False

    def _less_than(self, process_state : ProcessState, arg1 : int, arg2 : int, target_index : int) -> bool:
        process_state.memory.write(target_index, 1 if arg1 < arg2 else 0)
        process_state.instruction_pointer = process_state.instruction_pointer + 4
        return False

    def _equal(self, process_state : ProcessState, arg1 : int, arg2 : int, target_index : int) -> bool:
        process_state.memory.write(target_index, 1 if arg1 == arg2 else 0)
        process_state.instruction_pointer = process_state.instruction_pointer + 4
        return False

    def _set_base_pointer(self, process_state : ProcessState, target_index : int) -> bool:
        process_state.base_pointer = process_state.base_pointer + target_index
        process_state.instruction_pointer = process_state.instruction_pointer + 2
        return False


class Droid:
    def __init__(self, processor : BlockingIntCodeProcessor, code : IntCode):
        self._processor = processor
        self._state = processor.execute_code(code, [])

    def output(self) -> str:
        def to_string(code : IntCode) -> str:
            return "".join([chr(n) for n in code])
        return to_string(self._state.output_since_blocking_last)

    def take_input(self, input : str):
        def to_int_code_command(string : str) -> IntCode:
            return [ord(c) for c in string] + [ord('\n')]
        input_code = to_int_code_command(input)
        self._state.add_input(input_code)
        self._processor.continue_execution(self._state)



def main():
    input_reader = IntCodeReader()
    processor = BlockingIntCodeProcessor()
    input_file = "Advent20191225_1_input.txt"
    code = input_reader.int_code(input_file)
    droid = Droid(processor, code)
    _take_all_from_start(droid)
    print(droid.output())
    _find_right_items(droid)
    current_command = ""
    print(droid.output())
    while current_command != "exit":
        if current_command != "":
            droid.take_input(current_command)
            print(droid.output())
        current_command = input()

def _take_all_from_start(droid : Droid):
    commands = ["south",
        "east",
        "take boulder",
        "west",
        "north",
        "west",
        "take hypercube",
        "west",
        "take space law space brochure",
        "west",
        "north",
        "take shell",
        "west",
        "take mug",
        "south",
        "take festive hat",
        "north",
        "east",
        "south",
        "east",
        "east",
        "east",
        "east",
        "north",
        "west",
        "north",
        "take whirled peas",
        "west",
        "west",
        "take astronaut ice cream",
        "south"]
    for command in commands:
        print(droid.output())
        droid.take_input(command)

def _find_right_items(droid : Droid):
    def try_drop_an_item(droid : Droid, all_items : List[str], dropped_items : List[str]) -> bool:
        for item in all_items:
            if not item in dropped_items:
                droid.take_input("drop " + item)
                droid.take_input("south")
                output = droid.output()
                print(output)
                if not "Alert!" in output:
                    return True
                if "lighter" in output:
                    if try_drop_an_item(droid, all_items, dropped_items + [item]):
                        return True
                droid.take_input("take " + item)
        return False

    items = ["hypercube",
        "space law space brochure",
        "boulder",
        "shell",
        "astronaut ice cream",
        "mug",
        "whirled peas"]
    try_drop_an_item(droid, items, [])



if __name__ == "__main__":
    main()