from __future__ import annotations
from enum import Enum, IntEnum, auto
from typing import List, Dict, Tuple, Any, IO, Optional, Set, Callable
import re
import operator
import functools
import time

class ShuffleMove(Enum):
    Reverse = 1
    Cut = 2
    DealWithIncrement = 3


ShuffleInstruction = Tuple[ShuffleMove, Optional[int]]
ShuffleSequence = List[ShuffleInstruction]

class ShuffleSequenceReader:
    _instruction_pattern = r"([^0-9\-]+)(-?\d+)?"

    _moves_by_instruction_text = {
        "deal into new stack": ShuffleMove.Reverse,
        "cut": ShuffleMove.Cut,
        "deal with increment": ShuffleMove.DealWithIncrement,
        }

    def shuffle_instructions(self, filename : str) -> IO[ShuffleSequence]:
        file_contents = self._read_input(filename)
        lines = file_contents.splitlines()
        return [self._to_shuffle_instruction(line) for line in lines]

    def _read_input(self, filename : str) -> IO[str]:
        with open(filename, "r") as file:
            contents = file.read()
        return contents

    def _to_shuffle_instruction(self, instruction_line : str) -> ShuffleInstruction:
        instruction_matches = re.match(self._instruction_pattern, instruction_line)
        (move_text, raw_argument) = instruction_matches.group(1, 2)
        argument = int(raw_argument) if not raw_argument is None else None
        move = self._to_shuffle_move(move_text.strip())
        return (move, argument)

    def _to_shuffle_move(self, move_text : str) -> ShuffleMove:
        return self._moves_by_instruction_text.get(move_text, None)

CardDeck = List[int]

class Shuffler:
    def shuffle(self, deck : CardDeck, shuffle_instructions : ShuffleSequence) -> CardDeck:
        number_of_cards = len(deck)
        combined_reversing_data = self.combined_reversed_instruction_data(number_of_cards, shuffle_instructions)
        original_indices = (self.original_index(number_of_cards, combined_reversing_data, index) for index in range(number_of_cards))
        return [deck[original_index] for original_index in original_indices]

    def original_index(self, number_of_cards :int, instruction_data : Tuple[int,int], index : int) -> int:
        (factor, offset) = instruction_data
        return (factor * index + offset) % number_of_cards

    def reverse_instructions(self, number_of_cards: int, instructions : ShuffleSequence) -> ShuffleSequence:
        return [self._reverse_instruction(number_of_cards, instruction) for instruction in reversed(instructions)]

    def _reverse_instruction(self, number_of_cards: int, instruction : ShuffleInstruction) -> ShuffleInstruction:
        (move, argument) = instruction
        if move == ShuffleMove.Reverse:
            return instruction
        if move == ShuffleMove.Cut:
            return (move, number_of_cards - argument)
        if move == ShuffleMove.DealWithIncrement:
            inverse_argument = self._multiplicative_inverse(argument, number_of_cards)
            return (move, inverse_argument)
        return None

    def _multiplicative_inverse(self, element : int, base : int) -> int:
        (gcd, inverse_of_base, inverse_of_element) = self._extended_euklid(base, element)
        return inverse_of_element

    def _extended_euklid(self, a : int, b: int) -> Tuple[int,int,int]:
        def euklidean_step(previous_execution_row : Tuple[int,int,int], execution_row : Tuple[int,int,int]) -> Tuple[int,int,int]:
            (r0, s0, t0) = previous_execution_row
            (r1, s1, t1) = execution_row
            r = r0 % r1
            q = r0 // r1
            s = s0 - q * s1
            t = t0 - q * t1
            return (r, s, t)
        previous_row = (a, 1, 0)
        current_row = (b, 0, 1)
        while not current_row[0] == 0:
            next_row = euklidean_step(previous_row, current_row)
            previous_row = current_row
            current_row = next_row
        return previous_row

    # Factor and offset
    def combined_reversed_instruction_data(self, number_of_cards : int, shuffle_instructions : ShuffleSequence) -> Tuple[int, int]:
        reversing_instructions = self.reverse_instructions(number_of_cards, shuffle_instructions)
        (factor, offset) = (1, 0)
        for instruction in reversing_instructions:
            (instruction_factor, instruction_offset) = self._instruction_data(instruction)
            offset = (instruction_factor * offset + instruction_offset) % number_of_cards
            factor = (factor * instruction_factor) % number_of_cards
        return (factor, offset)

    def _instruction_data(self, instruction : ShuffleInstruction) -> Tuple[int, int]:
        (move, argument) = instruction
        if move == ShuffleMove.Reverse:
            return self._reverse_data()
        if move == ShuffleMove.Cut:
            return self._cut_data(argument)
        if move == ShuffleMove.DealWithIncrement:
            return self._deal_with_increment_data(argument)
        return None

    def _reverse_data(self) -> Tuple[int, int]:
        return (-1, -1)

    def _cut_data(self, N : int) -> Tuple[int, int]:
        return (1, -N)

    def _deal_with_increment_data(self, N : int) -> Tuple[int, int]:
        return (N, 0)

    # Factor and offset
    def combined_reversed_instruction_data_for_repeated_execution(self, number_of_cards : int, shuffle_instructions : ShuffleSequence, number_of_executions : int) -> Tuple[int, int]:
        base_instruction_data = self.combined_reversed_instruction_data(number_of_cards, shuffle_instructions)
        return self._data_for_repeated_execution(number_of_cards, base_instruction_data, number_of_executions)

    def _data_for_repeated_execution(self, number_of_cards : int, instruction_data : Tuple[int,int], number_of_executions : int) -> Tuple[int, int]:
        (factor, offset) = instruction_data
        execution_count = 1
        while execution_count * 2 <= number_of_executions:
            offset = (factor * offset + offset) % number_of_cards
            factor = (factor * factor) % number_of_cards
            execution_count = execution_count * 2
        remaining_executions = number_of_executions - execution_count
        if remaining_executions == 0:
            return (factor, offset)
        (other_factor, other_offset) = self._data_for_repeated_execution(number_of_cards, instruction_data, remaining_executions)
        offset = (other_factor * offset + other_offset) % number_of_cards
        factor = (other_factor * factor) % number_of_cards
        return (factor, offset)



def main():
    shuffler = Shuffler()
    input_reader = ShuffleSequenceReader()
    input_file = "Advent20191222_1_input.txt"
    shuffle_instructions = input_reader.shuffle_instructions(input_file)
    number_of_cards = 119315717514047
    number_of_executions = 101741582076661
    reverse_instruction_data = shuffler.combined_reversed_instruction_data_for_repeated_execution(number_of_cards, shuffle_instructions, number_of_executions)
    final_index = 2020
    initial_index = shuffler.original_index(number_of_cards, reverse_instruction_data, final_index)
    print(initial_index)



if __name__ == "__main__":
    main()