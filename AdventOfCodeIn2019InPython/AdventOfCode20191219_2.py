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




Point = Tuple[int,int]

class DroneState(IntEnum):
    Stationary = 0
    BeingPulled = 1


class Drone:
    def __init__(self, processor : BlockingIntCodeProcessor, code : IntCode):
        self._processor = processor
        self._code = code

    def move(self, position : Point) -> DroneState:
        (x,y) = position
        state = self._processor.execute_code(self._code.copy(), [y, x])
        return state.output_since_blocking_last[0]

    def map_square(self, width : int, x_offset : int = 0, y_offset : int = 0) -> Dict[Point, DroneState]:
        mapped_tiles : Dict[Point, DroneState] = {}
        for y in range(y_offset, y_offset + width):
            for step in range(x_offset, x_offset + width):
                x = step if y % 2 == 0 else width - step - 1
                tile_state = self.move((x,y))
                if tile_state != DroneState.Stationary:
                    mapped_tiles[(x,y)] = tile_state
        return mapped_tiles

    def show_square(self, width : int, x_offset : int = 0, y_offset : int = 0) -> str:
        mapped_square = self.map_square(width, x_offset, y_offset)
        return self.show_mapped_area(mapped_square, width, x_offset, y_offset)

    def show_mapped_area(self, mapped_area : Dict[Point, DroneState], width : int, x_offset : int = 0, y_offset : int = 0) -> str:
        (minimal_x, minimal_y, width, height) = (x_offset, y_offset, width, width)
        rows = [self._show_row(mapped_area, row_index, width, minimal_x, minimal_y) for row_index in range(height)]
        return "\n".join(rows)

    def _show_row(self, mapped_area : Dict[Point, DroneState], row_index : int, width : int, horizontal_offset : int, vertical_offset : int) -> str:
        display_items = [mapped_area.get((column_index + horizontal_offset, row_index + vertical_offset), DroneState.Stationary) for column_index in range(width)]
        character_representations = [self._character_representation(item) for item in display_items]
        return "".join(character_representations)

    def _character_representation(self, tile_state : DroneState) -> str:
        if tile_state == DroneState.Stationary:
            return " "
        if tile_state == DroneState.BeingPulled:
            #return "\u2588"
            return "#"
        return tile_state

    def is_on_beam(self, point : Point) -> bool:
        return self.move(point) == DroneState.BeingPulled

    def is_on_right_edge_of_beam(self, point : Point) -> bool:
        (x,y) = point
        return self.is_on_beam(point) and not self.is_on_beam((x+1,y))

    def is_on_upper_right_edge_of_beam(self, point : Point) -> bool:
        (x,y) = point
        return self.is_on_beam(point) and not self.is_on_beam((x+1,y)) \
            and not self.is_on_beam((x,y-1)) and not self.is_on_beam((x,y-2))

    def is_on_left_edge_of_beam(self, point : Point) -> bool:
        (x,y) = point
        return self.is_on_beam(point) and not self.is_on_beam((x-1,y))

    def is_on_lower_left_edge_of_beam(self, point : Point) -> bool:
        (x,y) = point
        return self.is_on_beam(point) and not self.is_on_beam((x-1,y)) \
            and not self.is_on_beam((x,y+1)) and not self.is_on_beam((x,y+2))

    def nearest_corner_of_fitted_square(self, width : int) -> Point:
        first_x = 100
        first_left_y_estimate = 287
        first_right_y_estimate = 208
        left_slope_estimate = self._estimated_left_slope(first_x, first_left_y_estimate)
        right_slope_estimate = self._estimated_right_slope(first_x, first_right_y_estimate)
        fitting_x_estimate = self._fit_square_horizontal_estimate(left_slope_estimate, right_slope_estimate, width)
        fitting_left_y_estimate_base = int(left_slope_estimate * fitting_x_estimate) + 10 # We want to rather overshoot to speed up finding the actual y.
        (fitting_x_estimate, fitting_left_y_estimate) = self._find_lower_left_edge_of_beam(fitting_x_estimate, fitting_left_y_estimate_base)
        return self._search_fit_around_estimate(fitting_x_estimate, fitting_left_y_estimate, width)

    def _estimated_left_slope(self, sample_x : int, y_estimate : int) -> float:
        (x,y) = self._find_lower_left_edge_of_beam(sample_x, y_estimate)
        return y/x

    def _estimated_right_slope(self, sample_x : int, y_estimate : int) -> float:
        (x,y) = self._find_upper_right_edge_of_beam(sample_x, y_estimate)
        return y/x

    def _find_lower_left_edge_of_beam(self, x_estimate : int, y_estimate : int) -> Point:
        has_seen_beam = False
        y_base = y_estimate
        x = x_estimate
        while not has_seen_beam:
            if self.is_on_beam((x,y_base)) and not self.is_on_lower_left_edge_of_beam((x,y_base)):
                y_base = 2 * y_base
            for y in range(y_base, 0, -1):
                if self.is_on_left_edge_of_beam((x,y)):
                    return (x,y)
                if not has_seen_beam and self.is_on_beam((x,y)):
                    has_seen_beam = True
            y_base = 2 * y_base
        y = y_estimate
        x_base = x_estimate
        while True:
            for x in range(x_estimate, 0, -1):
                if self.is_on_lower_left_edge_of_beam((x,y)):
                    return (x,y)
            x_base = 2 * x_base

    def _find_upper_right_edge_of_beam(self, x_estimate : int, y_estimate : int) -> Point:
        has_seen_beam = False
        y_base = y_estimate
        x = x_estimate
        while not has_seen_beam:
            for y in range(y_base, 0, -1):
                if self.is_on_upper_right_edge_of_beam((x,y)):
                    return (x,y)
                if not has_seen_beam and self.is_on_beam((x,y)):
                    has_seen_beam = True
            y_base = 2 * y_base
        y = y_estimate
        x_base = x_estimate
        while True:
            if self.is_on_beam((x_base,y)) and not self.is_on_upper_right_edge_of_beam((x_base,y)):
                y_base = 2 * y_base
            for x in range(x_estimate, 0, -1):
                if self.is_on_right_edge_of_beam((x,y)):
                    return (x,y)
            x_base = 2 * x_base

    def _fit_square_horizontal_estimate(self, left_slope_estimate : float, right_slope_estimate : float, square_width : int) -> int:
        return int(square_width * (1+right_slope_estimate)/(left_slope_estimate - right_slope_estimate))

    def _search_fit_around_estimate(self, x_estimate : int, left_y_estimate : int, width : int) -> Point:
        left_slope = left_y_estimate/x_estimate
        (x,y) = self._find_lower_left_edge_of_beam(x_estimate, left_y_estimate)
        while self._can_fit_square((x,y), width):
            x = x - 1
            y = int(y - left_slope + 2)
            (x,y) = self._find_lower_left_edge_of_beam(x, y)
        while not self._can_fit_square((x,y), width):
            x = x + 1
            y = int(y + left_slope + 2)
            (x,y) = self._find_lower_left_edge_of_beam(x, y)
        while self._can_fit_square((x,y), width):
            y = y - 1
        return (x, y - (width - 1) + 1)

    def _can_fit_square(self, bottom_lower_corner : Point, width : int) -> bool:
        (x,y) = bottom_lower_corner
        upper_right_corner = (x+(width-1), y-(width-1))
        return (self.is_on_beam(bottom_lower_corner) or self.is_on_beam((x, y+1))) \
            and (self.is_on_beam(upper_right_corner) or self.is_on_beam((x+(width-1), y-(width-1)-1)))



def main():
    input_reader = IntCodeReader()
    processor = BlockingIntCodeProcessor()
    input_file = "Advent20191219_1_input.txt"
    code = input_reader.int_code(input_file)
    drone = Drone(processor, code)
    closest_fit_corner = drone.nearest_corner_of_fitted_square(100)
    print(closest_fit_corner)
    (x,y) = closest_fit_corner
    print(x * 10000 + y)

def _write_to_file(filename : str, contents : str) -> IO:
        with open(filename, "w") as file:
            file.write(contents)


if __name__ == "__main__":
    main()