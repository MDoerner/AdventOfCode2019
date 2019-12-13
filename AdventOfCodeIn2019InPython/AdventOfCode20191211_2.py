from __future__ import annotations
from enum import Enum, IntEnum, auto
from itertools import permutations
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


class ProcessorNodeConfiguration:
    def __init__(self, node_id : Any, code : IntCode, phase_setting : int, target_node_ids):
        self.node_id = node_id
        self.code = code
        self.phase_setting = phase_setting
        self.target_node_ids = target_node_ids


class GraphProcessor:
    def __init__(self):
        self._processor = BlockingIntCodeProcessor()

    def graph_output(self, code : IntCode, node_configurations : List[ProcessorNodeConfiguration], start_node : Any, start_input : List[int], output_node : Any) -> List[int]:
        process_configs = {config.node_id: config for config in node_configurations}
        process_states = self._initialize_nodes(process_configs)
        execution_queue = queue.Queue()
        execution_queue.put_nowait((start_node, start_input))
        while not execution_queue.empty():
            self._process_node(execution_queue.get_nowait(), execution_queue, process_states, process_configs)
        return process_states[output_node].output_since_blocking_last

    def _initialize_nodes(self, process_configurations : Dict[Any, ProcessorNodeConfiguration]) -> Dict[Any, ProcessState]:
        return {node_id: self._initialize_node(process_configurations[node_id]) for node_id in process_configurations.keys()}

    def _initialize_node(self, process_configuration : ProcessorNodeConfiguration) -> ProcessState:
        return self._processor.execute_code(process_configuration.code, [process_configuration.phase_setting])

    def _process_node(self, execution_data : Tuple[int, List[int]], execution_queue : queue.Queue, process_states : Dict[Any, ProcessState], process_configs : Dict[Any, ProcessorNodeConfiguration]):
        (node_id, node_input) = execution_data
        node_state = process_states[node_id]
        node_state.add_input(node_input)
        self._processor.continue_execution(node_state)
        self._enqueue_dependent_tasks(node_id, node_state.output_since_blocking_last, process_states, process_configs, execution_queue)

    def _enqueue_dependent_tasks(self, processed_node : Any, input_list : List[int], process_states : Dict[Any, ProcessState], process_configs : Dict[Any, ProcessorNodeConfiguration], execution_queue : queue.Queue):
        target_nodes = process_configs[processed_node].target_node_ids
        self._enqueue_tasks(target_nodes, input_list, execution_queue, process_states)

    def _enqueue_tasks(self, target_nodes : List[Any], input_list : List[int], execution_queue : queue.Queue, process_states : Dict[Any, ProcessState]):
        for target_node in target_nodes:
            if not process_states[target_node].has_shut_down():
                execution_queue.put_nowait((target_node, input_list))


class Color (IntEnum):
    Black = 0
    White = 1


Point = Tuple[int, int]

class Hull:
    def __init__(self):
        self._tile_colors : Dict[Point, Color] = {}

    def tile_color(self, point : Point) -> Color:
        return self._tile_colors.get(point, 0)

    def paint_tile(self, point : Point, color : Color):
        self._tile_colors[point] = color

    def number_of_painted_tiles(self) -> int:
        return len(self._tile_colors)

    def show(self) -> str:
        (minimal_x, minimal_y, width, height) = self._dimensions()
        rows = [self._show_row(row_index, width, minimal_x, minimal_y) for row_index in range(height)]
        return "\n".join(rows)

    def _show_row(self, row_index : int, width : int, horizontal_offset : int, vertical_offset : int) -> str:
        colors = [self.tile_color((column_index + horizontal_offset, row_index + vertical_offset)) for column_index in range(width)]
        character_representations = [self._character_representation(color) for color in colors]
        return "".join(character_representations)        

    def _character_representation(self, color : int) -> str:
        if color == Color.Black:
            return " "
        if color == Color.White:
            return "\u2588"
        return "x"

    def _dimensions(self) -> Tuple[int, int, int, int]:
        non_black_tiles = [point for point in self._tile_colors if self.tile_color(point) != Color.Black]
        x_coordinates = [x for (x,y) in non_black_tiles]
        y_coordinates = [y for (x,y) in non_black_tiles]
        minimal_x = min(x_coordinates)
        width = max(x_coordinates) - minimal_x + 1
        minimal_y = min(y_coordinates)
        height = max(y_coordinates) - minimal_y + 1
        return (minimal_x, minimal_y, width, height)


class Direction(IntEnum):
    Up = 0
    Right = 1
    Down = 2
    Left = 3

    def next_clockwise(self) -> Direction:
        return self._to_direction((self.value + 1) % 4)

    def next_counterclockwise(self) -> Direction:
        return self._to_direction((self.value - 1) % 4)

    def _to_direction(self, number : int):
        if number == Direction.Up:
            return Direction.Up
        if number == Direction.Down:
            return Direction.Down 
        if number == Direction.Left:
            return Direction.Left 
        if number == Direction.Right:
            return Direction.Right 
        return None


class PaintingRobot:
    def __init__(self, int_code_processor : BlockingIntCodeProcessor):
        self._intCodeProcessor = int_code_processor

    def paint_hull(self, code : IntCode, start_point : Point, start_direction : Direction, hull : Hull):
        position = start_point
        direction = start_direction
        process_state = self._intCodeProcessor.execute_code(code, [])
        while not process_state.has_shut_down():
            (position, direction) = self._paint_next_tile(position, direction, process_state, hull)

    # Returns the new position and direction.
    def _paint_next_tile(self, position : Point, direction : Direction, process_state : ProcessState, hull : Hull) -> Tuple[Point, Direction]:
        current_color = hull.tile_color(position)
        process_state.add_input([current_color])
        self._intCodeProcessor.continue_execution(process_state)
        outputs = process_state.output_since_blocking_last
        color = outputs[0]
        turn_direction = outputs[1]
        hull.paint_tile(position, color)
        new_position_data = self._move(position, direction, turn_direction)
        return new_position_data

    def _move(self, position : Point, direction: Direction, turn_description : int) -> Tuple[Point, Direction]:
        new_direction = self._turn(direction, turn_description)
        new_position =  self._move_forward(position, new_direction)
        return (new_position, new_direction)
        
    def _turn(self, direction: Direction, turn_description : int) -> Direction:
        if turn_description == 0:
            return direction.next_counterclockwise()
        elif turn_description == 1:
            return direction.next_clockwise()
        else:
            return direction

    def _move_forward(self, position : Point, direction : Direction) -> Point:
        (x, y) = position
        if direction == Direction.Up:
            return (x, y - 1)
        if direction == Direction.Down:
            return (x, y + 1)
        if direction == Direction.Left:
            return (x - 1, y)
        if direction == Direction.Right:
            return (x + 1, y)
        return None



def main():
    input_reader = IntCodeReader()
    processor = BlockingIntCodeProcessor()
    painting_robot = PaintingRobot(processor)
    hull = Hull()
    input_file = "Advent20191211_1_input.txt"
    code = input_reader.int_code(input_file)
    hull.paint_tile((0,0), Color.White)
    painting_robot.paint_hull(code, (0, 0), Direction.Up, hull)
    hull_painting = hull.show()
    print(hull_painting)


if __name__ == "__main__":
    main()