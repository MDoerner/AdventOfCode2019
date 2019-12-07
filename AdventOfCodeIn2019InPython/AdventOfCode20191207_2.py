from enum import IntEnum
from itertools import permutations
import queue

class OpCode(IntEnum):
    ADD = 1
    MULTIPLY = 2
    GET = 3
    PUT = 4
    JUMP_IF_TRUE = 5
    JUMP_IF_FALSE = 6
    LESS_THEN = 7
    EQUALS = 8
    STOP = 99


class ArgumentMode(IntEnum):
    PONTER = 0
    VALUE = 1


class IntCodeInstruction:
    _number_of_arguments = {
        OpCode.ADD: 3,
        OpCode.MULTIPLY: 3,
        OpCode.GET: 1,
        OpCode.PUT: 1,
        OpCode.JUMP_IF_TRUE: 2,
        OpCode.JUMP_IF_FALSE: 2,
        OpCode.LESS_THEN: 3,
        OpCode.EQUALS: 3,
        OpCode.STOP: 0
    }

    _write_arguments = {
        OpCode.ADD: [2],
        OpCode.MULTIPLY: [2],
        OpCode.GET: [0],
        OpCode.PUT: [],
        OpCode.JUMP_IF_TRUE: [],
        OpCode.JUMP_IF_FALSE: [],
        OpCode.LESS_THEN: [2],
        OpCode.EQUALS: [2],
        OpCode.STOP: []
    }

    def __init__(self, instruction):
        self.opcode = self._op_code(instruction)
        self.argument_modes = self._argument_modes(instruction // 100, self.opcode)

    def number_of_arguments(self):
        return len(self.argument_modes)

    def _op_code(self, instruction):
        return instruction % 100

    def _argument_modes(self, argument_mode_specification, opcode):
        number_of_arguments = self._number_of_arguments[opcode]
        if number_of_arguments == 0:
            return []
        modes = self._visible_argument_modes(argument_mode_specification)
        self._fill_modes_for_obitted_specification(modes, number_of_arguments)
        self._make_write_arguments_value_arguments(modes, opcode)
        return modes

    def _visible_argument_modes(self, argument_mode_specification):
        modes = self._to_digits(argument_mode_specification)
        return list(reversed(modes))

    def _to_digits(self, natural_number):
        return [int(d) for d in str(natural_number)]

    def _fill_modes_for_obitted_specification(self, modes, number_of_arguements):
        number_of_missing_arguments = number_of_arguements - len(modes)
        if number_of_missing_arguments > 0:
            missing_modes = [0 for x in range(number_of_missing_arguments)]
            modes.extend(missing_modes)

    # Although the challange says they are not immediate arguments, they behave like those
    # if you consider the target address pointer to be the value. 
    # The pinter is not at the place the value points to.
    def _make_write_arguments_value_arguments(self, modes, opcode):
        write_arguments = self._write_arguments[opcode]
        for index in write_arguments:
            modes[index] = ArgumentMode.VALUE


class IntCodeReader:
    def int_code(self, filename):
        input_string = self._read_input(filename)
        return self._int_code(input_string)

    def _read_input(self, filename):
        with open(filename, "r") as file:
            contents = file.read()
        return contents

    def _int_code(self, input_string):
        return [int(x) for x in input_string.split(',')]


class BlockingIntCodeProcessor:
    def execute_code(self, code, input_stream):
        process_state = ProcessState(code, 0, input_stream, [], [])
        return self.continue_execution(process_state)

    def continue_execution(self, process_state):
        code = process_state.code
        instruction_pointer = process_state.instruction_pointer
        input_stream = process_state.input_stream
        output = []
        instruction_blocks = False
        while instruction_pointer != None and not instruction_blocks:
            current_instruction = self._invocation_data(code, instruction_pointer)
            (new_output, instruction_pointer, instruction_blocks) = self._execute_operation(code, instruction_pointer, current_instruction, input_stream)
            output = output + new_output
        return ProcessState(code, instruction_pointer, input_stream, process_state.output + output, output)

    def _invocation_data(self, code, instruction_pointer):
        instruction = code[instruction_pointer]
        return IntCodeInstruction(instruction)

    def _execute_operation(self, code, instruction_pointer, instruction, input_stream):
        opcode = instruction.opcode 
        arguments = self._operation_arguments(code, instruction_pointer, instruction.argument_modes)
        if opcode == OpCode.STOP:
            return self._stop()
        if opcode == OpCode.ADD:
            return self._add(code, instruction_pointer, arguments[0], arguments[1], arguments[2])
        elif opcode == OpCode.MULTIPLY:
            return self._multiply(code, instruction_pointer, arguments[0], arguments[1], arguments[2])
        elif opcode == OpCode.GET:
            return self._get(code, instruction_pointer, input_stream, arguments[0])
        elif opcode == OpCode.PUT:
            return self._put(code, instruction_pointer, arguments[0])
        elif opcode == OpCode.JUMP_IF_TRUE:
            return self._jump_if_true(code, instruction_pointer, arguments[0], arguments[1])
        elif opcode == OpCode.JUMP_IF_FALSE:
            return self._jump_if_false(code, instruction_pointer, arguments[0], arguments[1])
        elif opcode == OpCode.LESS_THEN:
            return self._less_than(code, instruction_pointer, arguments[0], arguments[1], arguments[2])
        elif opcode == OpCode.EQUALS:
            return self._equal(code, instruction_pointer, arguments[0], arguments[1], arguments[2])
        return None

    def _operation_arguments(self, code, instruction_pointer, argument_modes):
        return [self._operation_argument(code, instruction_pointer + argument_index + 1, argument_mode) for (argument_index, argument_mode) in enumerate(argument_modes)]

    def _operation_argument(self, code, argument_pointer, argument_mode):
        if argument_mode == ArgumentMode.PONTER:
            return code[code[argument_pointer]]
        if argument_mode == ArgumentMode.VALUE:
            return code[argument_pointer]
        return None

    def _stop(self):
        return ([], None, False)

    def _add(self, code, instruction_pointer, arg1, arg2, target_index):
        code[target_index] = arg1 + arg2
        return ([], instruction_pointer + 4, False)

    def _multiply(self, code, instruction_pointer, arg1, arg2, target_index):
        code[target_index] = arg1 * arg2
        return ([], instruction_pointer + 4, False)

    def _get(self, code, instruction_pointer, input_stream, target_index):
        if not input_stream:
            return ([], instruction_pointer, True)
        code[target_index] = input_stream.pop()
        return ([], instruction_pointer + 2, False)

    def _put(self, code, instruction_pointer, value):
        return ([value], instruction_pointer + 2, False)

    def _jump_if_true(self, code, instruction_pointer, value, target_index):
        if value != 0:
            return ([], target_index, False)
        return ([], instruction_pointer + 3, False)

    def _jump_if_false(self, code, instruction_pointer, value, target_index):
        if value == 0:
            return ([], target_index, False)
        return ([], instruction_pointer + 3, False)

    def _less_than(self, code, instruction_pointer, arg1, arg2, target_index):
        code[target_index] = 1 if arg1 < arg2 else 0
        return ([], instruction_pointer + 4, False)

    def _equal(self, code, instruction_pointer, arg1, arg2, target_index):
        code[target_index] = 1 if arg1 == arg2 else 0
        return ([], instruction_pointer + 4, False)

class ProcessState:
    def __init__(self, code, instruction_pointer, input_stream, output, output_since_blocking_last):
        self.code = code
        self.instruction_pointer = instruction_pointer
        self.input_stream = input_stream
        self.output = output
        self.output_since_blocking_last = output_since_blocking_last

    def has_shut_down(self):
        return self.instruction_pointer is None

    def add_input(self, input_list):
        self.input_stream = [item for item in reversed(input_list)] + self.input_stream

class ProcessorNodeConfiguration:
    def __init__(self, node_id, code, phase_setting, target_node_ids):
        self.node_id = node_id
        self.code = code
        self.phase_setting = phase_setting
        self.target_node_ids = target_node_ids

class GraphProcessor:
    def __init__(self):
        self._processor = BlockingIntCodeProcessor()

    def graph_output(self, code, node_configurations, start_node, start_input, output_node):
        process_configs = {config.node_id: config for config in node_configurations}
        process_states = self._initialize_nodes(process_configs)
        execution_queue = queue.Queue()
        execution_queue.put_nowait((start_node, start_input))
        while not execution_queue.empty():
            self._process_node(execution_queue.get_nowait(), execution_queue, process_states, process_configs)
        return process_states[output_node].output_since_blocking_last

    def _initialize_nodes(self, process_configurations):
        return {node_id: self._initialize_node(process_configurations[node_id]) for node_id in process_configurations.keys()}

    def _initialize_node(self, process_configuration):
        return self._processor.execute_code(process_configuration.code, [process_configuration.phase_setting])

    def _process_node(self, execution_data, execution_queue, process_states, process_configs):
        (node_id, node_input) = execution_data
        node_state = process_states[node_id]
        node_state.add_input(node_input)
        stop_state = self._processor.continue_execution(node_state)
        process_states[node_id] = stop_state
        self._enqueue_dependent_tasks(node_id, stop_state.output_since_blocking_last, process_states, process_configs, execution_queue)

    def _enqueue_dependent_tasks(self, processed_node, input_list, process_states, process_configs, execution_queue):
        target_nodes = process_configs[processed_node].target_node_ids
        self._enqueue_tasks(target_nodes, input_list, execution_queue, process_states)

    def _enqueue_tasks(self, target_nodes, input_list, execution_queue, process_states):
        for target_node in target_nodes:
            if not process_states[target_node].has_shut_down():
                execution_queue.put_nowait((target_node, input_list))


def main():
    input_reader = IntCodeReader()
    processor = GraphProcessor()
    input_file = "Advent20191207_1_input.txt"
    code = input_reader.int_code(input_file)
    output = _maximize_output(code, processor, range(5, 10))
    print(output)

def _maximize_output(code, graph_processor, phase_settings):
    possible_phase_settings = permutations(phase_settings)
    possible_configurations = [_graph_configurartion(code.copy(), candidate_phase_setting) for candidate_phase_setting in possible_phase_settings]
    outputs = [graph_processor.graph_output(code, candidate_configuration, 0, [0], 4) for candidate_configuration in possible_configurations]
    last_output_values = [individual_output[-1] for individual_output in outputs]
    return max(last_output_values)

def _graph_configurartion(code, phase_settings):
    return [ProcessorNodeConfiguration(index, code, phase_setting, [0] if index == len(phase_settings) - 1 else [index + 1]) for index, phase_setting in enumerate(phase_settings)]


if __name__ == "__main__":
    main()