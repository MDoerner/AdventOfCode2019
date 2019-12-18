from __future__ import annotations
from enum import Enum, IntEnum, auto
from typing import List, Dict, Tuple, Any, IO, Optional, Iterable
import queue

Digits = List[int]

class  DigitSequenceReader:
    def digit_sequence(self, filename : str) -> IO[Digits]:
        input_string = self._read_input(filename)
        return self.to_digits(input_string)

    def _read_input(self, filename : str) -> IO[str]:
        with open(filename, "r") as file:
            contents = file.read()
        return contents

    def to_digits(self, input_string : str) -> Digits:
        return [int(x) for x in input_string.rstrip()]


class FFTProcessor:
    _pattern = [0,1,0,-1]

    def compute_phase(self, sequence : List[int]) -> List[int]:
        first_half = [self._compute_digit(sequence, self._pattern, index + 1) for index in range(len(sequence)//2+1)]
        second_half = self.fast_second_half_phase(sequence[len(sequence)//2+1:])
        return first_half + second_half

    def _compute_digit(self, sequence : List[int], pattern : List[int], digit_number : int) -> int:
        possibly_neccessary_repetitions = len(sequence)//len(pattern) + 1
        iteration_pattern = self._iteration_pattern(pattern, possibly_neccessary_repetitions, digit_number)
        next(iteration_pattern) # Skip first element.
        number = sum([x*y for x,y in zip(iteration_pattern, sequence)])
        return number % 10 if number >= 0 else -number % 10

    def _iteration_pattern(self, pattern: List[int], repetitions : int, iteration_number: int) -> Iterable[int]:
        for n in range(repetitions):
            for item in pattern:
                for m in range(iteration_number):
                    yield item

    def _to_digits(self, input_string : int) -> Digits:
        return [int(x) for x in str(input_string) if x != '-']

    # In the second half of the sequence, the pattern is enlarged and truncated such that
    # it consists of zeros before the index and ones thereafter.
    def fast_second_half_phase(self, sequence : List[int]) -> List[int]:
        reversed_output = []
        current_digit = 0
        for digit in reversed(sequence):
            current_digit = (current_digit + digit) % 10
            reversed_output.append(current_digit)
        return list(reversed(reversed_output))



def main():
    fft_processor = FFTProcessor()
    input_reader = DigitSequenceReader()
    input_file = "Advent20191216_1_input.txt"
    digit_sequence = input_reader.digit_sequence(input_file)
    offset = int("".join([str(x) for x in digit_sequence[:7]]))
    print(offset)
    digit_sequence = list(_repeated_sequence(digit_sequence, 10000))
    print(len(digit_sequence))
    relevant_digits = digit_sequence[offset:]
    for n in range(100):
        relevant_digits = fft_processor.fast_second_half_phase(relevant_digits)
    result = int("".join([str(x) for x in relevant_digits[:8]]))
    print(result)

def _repeated_sequence(sequence : List[int], times : int) -> Iterable[int]:
    for n in range(times):
        for item in sequence:
            yield item

if __name__ == "__main__":
    main()