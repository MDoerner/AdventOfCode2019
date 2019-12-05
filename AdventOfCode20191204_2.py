def main():
    start_number = 231832
    end_number = 767346
    print(number_of_passwords(start_number, end_number))

def number_of_passwords(start_number, end_number):
    return len(valid_passwords(start_number, end_number))

def valid_passwords(start_number, end_number):
    return [candidate for candidate in range(start_number, end_number + 1) if is_valid_password(candidate)]

def is_valid_password(natural_number):
    digit_list = to_digits(natural_number)
    return is_ascending(digit_list) and has_isolated_double(digit_list)

def to_digits(natural_number):
    return [int(d) for d in str(natural_number)]

def is_ascending(digit_list):
    for index in range(len(digit_list) - 1):
        if digit_list[index + 1] < digit_list[index]:
            return False
    return True

def has_isolated_double(digit_list):
    length_of_list = len(digit_list)
    for index in range(length_of_list - 1):
        if digit_list[index + 1] == digit_list[index]:
            if (index == 0 or digit_list[index - 1] != digit_list[index]) and \
                (index + 2 == length_of_list or digit_list[index + 2] != digit_list[index]):
                return True
    return False

if __name__ == "__main__":
    main()