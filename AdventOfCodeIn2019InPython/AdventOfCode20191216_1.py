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
    def compute_phase(self, sequence : List[int], pattern : List[int]) -> List[int]:
        return [self._compute_digit(sequence, pattern, index + 1) for index in range(len(sequence))]

    def _compute_digit(self, sequence : List[int], pattern : List[int], digit_number : int) -> int:
        possibly_neccessary_repetitions = len(sequence)//len(pattern) + 1
        iteration_pattern = self._iteration_pattern(pattern, possibly_neccessary_repetitions, digit_number)
        next(iteration_pattern) # Skip first element.
        number = sum([x*y for x,y in zip(iteration_pattern, sequence)])
        digits = self._to_digits(number)
        return digits[-1]

    def _iteration_pattern(self, pattern: List[int], repetitions : int, iteration_number: int) -> Iterable[int]:
        for n in range(repetitions):
            for item in pattern:
                for m in range(iteration_number):
                    yield item

    def _to_digits(self, input_string : int) -> Digits:
        return [int(x) for x in str(input_string) if x != '-']



def main():
    fft_processor = FFTProcessor()
    input_reader = DigitSequenceReader()
    input_file = "Advent20191216_1_input.txt"
    digit_sequence = input_reader.digit_sequence(input_file)
    pattern = [0,1,0,-1]
    for n in range(100):
        digit_sequence = fft_processor.compute_phase(digit_sequence, pattern)
    result = int("".join([str(x) for x in digit_sequence[:8]]))
    print(result)

if __name__ == "__main__":
    main()