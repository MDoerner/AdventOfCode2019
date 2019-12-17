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


class Direction(IntEnum):
    Down = 1
    Up = 2
    Left = 3
    Right = 4
    Falling = 99

    def opposite_direction(self):
        if self == Direction.Down:
            return Direction.Up
        if self == Direction.Up:
            return Direction.Down
        if self == Direction.Left:
            return Direction.Right
        if self == Direction.Right:
            return Direction.Left
        return None


Point = Tuple[int,int]
Path = List[Direction]


class DisplayItem(IntEnum):
    Empty = ord('.')
    Scaffold = ord('#')
    Robot_Up = ord('^')
    Robot_Down = ord('v')
    Robot_Left = ord('<')
    Robot_Right = ord('>')
    Robot_Falling = ord('X')
    LineBreak = ord('\n')


class Camera:
    def __init__(self):
        self._display_state : Dict[Point, DisplayItem] = {}
        self._robot_position = (-10,-10)
        self._robot_direction = Direction.Up

    def process_camera_output(self, camera_output : List[int]):
        (x, y) = (0, 0)
        for tile in camera_output:
            if tile == DisplayItem.LineBreak:
                (x, y) = (0, y + 1)
            else:
                if tile == DisplayItem.Empty:
                    if (x,y) in self._display_state:
                        del self._display_state[(x,y)]
                else:
                    self._display_state[(x, y)] = tile
                    if tile == DisplayItem.Robot_Up:
                        self._robot_position = (x,y)
                        self._robot_direction = Direction.Up
                    elif tile == DisplayItem.Robot_Down:
                        self._robot_position = (x,y)
                        self._robot_direction = Direction.Down
                    elif tile == DisplayItem.Robot_Left:
                        self._robot_position = (x,y)
                        self._robot_direction = Direction.Left
                    elif tile == DisplayItem.Robot_Right:
                        self._robot_position = (x,y)
                        self._robot_direction = Direction.Right
                    elif tile == DisplayItem.Robot_Falling:
                        self._robot_position = (x,y)
                        self._robot_direction = Direction.Falling
                x = x + 1

    def robot_position(self) -> Point:
        return self._robot_position

    def robot_direction(self) -> Direction:
        return self._robot_direction

    def display_state(self, point: Point) -> DisplayItem:
        return self._display_state.get(point, DisplayItem.Empty)

    def show(self) -> str:
        (minimal_x, minimal_y, width, height) = self._dimensions()
        rows = [self._show_row(row_index, width, minimal_x, minimal_y) for row_index in range(height)]
        return "\n".join(rows)

    def _show_row(self, row_index : int, width : int, horizontal_offset : int, vertical_offset : int) -> str:
        display_items = [self.display_state((column_index + horizontal_offset, row_index + vertical_offset)) for column_index in range(width)]
        character_representations = [self._character_representation(item) for item in display_items]
        return "".join(character_representations)

    def _character_representation(self, tile_id : DisplayItem) -> str:
        if tile_id == DisplayItem.Empty:
            return " "
        if tile_id == DisplayItem.Scaffold:
            return "\u2588"
        return chr(tile_id)

    def _dimensions(self) -> Tuple[int, int, int, int]:
        non_empty_tiles = [point for point in self._display_state if self._display_state[point] != DisplayItem.Empty]
        x_coordinates = [x for (x,y) in non_empty_tiles]
        y_coordinates = [y for (x,y) in non_empty_tiles]
        minimal_x = min(x_coordinates)
        width = max(x_coordinates) - minimal_x + 1
        minimal_y = min(y_coordinates)
        height = max(y_coordinates) - minimal_y + 1
        return (minimal_x, minimal_y, width, height)

    def alighnment_parameter_sum(self) -> int:
        return sum([x*y for x,y in self._intersections()])

    def _intersections(self) -> List[Point]:
        intersections = []
        for point in self._display_state:
            if all([p in self._display_state for p in self._adjacent_points(point)]):
                intersections.append(point)
        return intersections

    def _adjacent_points(self, point : Point) -> List[Point]:
        (x,y) = point
        return [(x-1,y), (x+1,y), (x,y-1), (x, y+1)]



def main():
    input_reader = IntCodeReader()
    processor = BlockingIntCodeProcessor()
    input_file = "Advent20191217_1_input.txt"
    code = input_reader.int_code(input_file)
    code[0] = 2
    main_input = [ord(c) for c in "A,B,A,B,C,C,B,A,C,A\n"]
    A_input = [ord(c) for c in "L,10,R,8,R,6,R,10\n"]
    B_input = [ord(c) for c in "L,12,R,8,L,12\n"]
    C_input = [ord(c) for c in "L,10,R,8,R,8\n"]
    video_mode_input = [ord(c) for c in "n\n"]
    process_state = processor.execute_code(code, [])
    process_state.add_input(main_input)
    processor.continue_execution(process_state)
    process_state.add_input(A_input)
    processor.continue_execution(process_state)
    process_state.add_input(B_input)
    processor.continue_execution(process_state)
    process_state.add_input(C_input)
    processor.continue_execution(process_state)
    process_state.add_input(video_mode_input)
    processor.continue_execution(process_state)
    print(process_state.output_since_blocking_last[-1])


if __name__ == "__main__":
    main()