from enum import IntEnum

class OpCode(IntEnum):
    ADD = 1
    MULTIPLY = 2
    GET = 3
    PUT = 4
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
        OpCode.STOP: 0
    }

    _write_arguments = {
        OpCode.ADD: [2],
        OpCode.MULTIPLY: [2],
        OpCode.GET: [0],
        OpCode.PUT: [],
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
            output = output + self._execute_operation(code, instruction_pointer, current_instruction, input_stream)
            instruction_pointer = instruction_pointer + 1 + current_instruction.number_of_arguments()
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
            return self._add(code, arguments[0], arguments[1], arguments[2])
        elif opcode == OpCode.MULTIPLY:
            return self._multiply(code, arguments[0], arguments[1], arguments[2])
        elif opcode == OpCode.GET:
            input_value = input_stream.pop()
            return self._get(code, input_value, arguments[0])
        elif opcode == OpCode.PUT:
            return self._put(code, arguments[0])
        return None

    def _operation_arguments(self, code, instruction_pointer, argument_modes):
        return [self._operation_argument(code, instruction_pointer + argument_index + 1, argument_mode) for (argument_index, argument_mode) in enumerate(argument_modes)]

    def _operation_argument(self, code, argument_pointer, argument_mode):
        if argument_mode == ArgumentMode.PONTER:
            return code[code[argument_pointer]]
        if argument_mode == ArgumentMode.VALUE:
            return code[argument_pointer]
        return None

    def _add(self, code, arg1, arg2, target_index):
        code[target_index] = arg1 + arg2
        return []

    def _multiply(self, code, arg1, arg2, target_index):
        code[target_index] = arg1 * arg2
        return []

    def _get(self, code, value, target_index):
        code[target_index] = value
        return []

    def _put(self, code, value):
        return [value]


def main():
    input_reader = IntCodeReader()
    processor = IntCodeProcessor()
    input_file = "Advent20191205_1_input.txt"
    input_stream = [1]
    code = input_reader.int_code(input_file)
    output = processor.execute_code(code, input_stream)
    print(output)


if __name__ == "__main__":
    main()