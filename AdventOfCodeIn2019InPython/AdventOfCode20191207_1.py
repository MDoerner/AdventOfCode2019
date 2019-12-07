from enum import IntEnum
from itertools import permutations

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


class IntCodeProcessor:

    def execute_code(self, code, input_stream):
        instruction_pointer = 0
        output = []
        current_instruction = self._invocation_data(code, instruction_pointer)
        while current_instruction.opcode != OpCode.STOP:
            (new_output, instruction_pointer) = self._execute_operation(code, instruction_pointer, current_instruction, input_stream)
            output = output + new_output
            current_instruction = self._invocation_data(code, instruction_pointer)
        return output
    
    def _invocation_data(self, code, instruction_pointer):
        instruction = code[instruction_pointer]
        return IntCodeInstruction(instruction)

    def _execute_operation(self, code, instruction_pointer, instruction, input_stream):
        opcode = instruction.opcode 
        arguments = self._operation_arguments(code, instruction_pointer, instruction.argument_modes)
        if opcode == OpCode.STOP:
            return []
        if opcode == OpCode.ADD:
            return self._add(code, instruction_pointer, arguments[0], arguments[1], arguments[2])
        elif opcode == OpCode.MULTIPLY:
            return self._multiply(code, instruction_pointer, arguments[0], arguments[1], arguments[2])
        elif opcode == OpCode.GET:
            input_value = input_stream.pop()
            return self._get(code, instruction_pointer, input_value, arguments[0])
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

    def _add(self, code, instruction_pointer, arg1, arg2, target_index):
        code[target_index] = arg1 + arg2
        return ([], instruction_pointer + 4)

    def _multiply(self, code, instruction_pointer, arg1, arg2, target_index):
        code[target_index] = arg1 * arg2
        return ([], instruction_pointer + 4)

    def _get(self, code, instruction_pointer, value, target_index):
        code[target_index] = value
        return ([], instruction_pointer + 2)

    def _put(self, code, instruction_pointer, value):
        return ([value], instruction_pointer + 2)

    def _jump_if_true(self, code, instruction_pointer, value, target_index):
        if value != 0:
            return ([], target_index)
        return ([], instruction_pointer + 3)

    def _jump_if_false(self, code, instruction_pointer, value, target_index):
        if value == 0:
            return ([], target_index)
        return ([], instruction_pointer + 3)

    def _less_than(self, code, instruction_pointer, arg1, arg2, target_index):
        code[target_index] = 1 if arg1 < arg2 else 0
        return ([], instruction_pointer + 4)

    def _equal(self, code, instruction_pointer, arg1, arg2, target_index):
        code[target_index] = 1 if arg1 == arg2 else 0
        return ([], instruction_pointer + 4)

class ArrayProcessor:
    def __init__(self):
        self._processor = IntCodeProcessor()

    def array_output(self, code, phase_settings):
        current_input = 0
        for phase_setting in phase_settings:
            output_list = self._processor.execute_code(code.copy(), [current_input, phase_setting])
            current_input = output_list[0]
        return current_input



def main():
    input_reader = IntCodeReader()
    processor = ArrayProcessor()
    input_file = "Advent20191207_1_input.txt"
    code = input_reader.int_code(input_file)
    output = _maximize_output(code, processor, range(5))
    print(output)

def _maximize_output(code, array_processor, phase_settings):
    possible_phase_settings = permutations(phase_settings)
    outputs = [array_processor.array_output(code, candidate_phase_setting) for candidate_phase_setting in possible_phase_settings]
    return max(outputs)


if __name__ == "__main__":
    main()