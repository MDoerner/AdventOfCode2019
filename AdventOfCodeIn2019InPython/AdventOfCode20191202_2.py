def main():
    input_string = read_input("Advent20191202_error_input.txt")
    code = int_code(input_string)
    (noun, verb) = reverse_output(code, 19690720)
    print(100 * noun + verb)

def read_input(filename):
    with open(filename, "r") as file:
        contents = file.read()
    return contents

def int_code(input_string):
    return [int(x) for x in input_string.split(',')]

def reverse_output(code, output):
    for noun in range(99):
        for verb in range(99):
            if execute(code, noun, verb) == output:
                return (noun, verb)
    return None

def execute(code, noun, verb):
    execution_code = code.copy()
    set_parameters(execution_code, noun, verb)
    return execute_code(execution_code)

def set_parameters(code, noun, verb):
    code[1] = noun
    code[2] = verb

def execute_code(code):
    execution_pointer = 0
    while op_code(code, execution_pointer) != 99:
        execute_operation(code, execution_pointer)
        execution_pointer = execution_pointer + 4
    return code[0]

def op_code(code, execution_pointer):
    opcode = code[execution_pointer]
    if opcode not in [1,2,99]:
        print("Unknown op code " + opcode + " at " + execution_pointer + ". (start = " + code[0] + ")")
        raise ValueError("Unknown op code", opcode, execution_pointer, code[0])
    return opcode

def execute_operation(code, execution_pointer):
    current_op_code = op_code(code, execution_pointer)
    if current_op_code == 99:
        return
    (op1, op2, target_ndex) = operation_arguments(code, execution_pointer)
    if current_op_code == 1:
        code[target_ndex] = op1 + op2
    elif current_op_code == 2:
        code[target_ndex] = op1 * op2

def operation_arguments(code, execution_pointer):
    first_operand = code[code[execution_pointer + 1]]
    second_operand = code[code[execution_pointer + 2]]
    target_index = code[execution_pointer + 3]
    return (first_operand, second_operand, target_index)



if __name__ == "__main__":
    main()