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
        original_index_fnc = self.original_index_reverse_fnc(number_of_cards, shuffle_instructions)
        original_indices = (original_index_fnc(index) for index in range(number_of_cards))
        end_time = time.time()
        return [deck[original_index] for original_index in original_indices]

    def original_index_reverse_fnc(self, number_of_cards : int, shuffle_instructions : ShuffleSequence) -> Callable[[int],int]:
        reverse_functions = [self._individual_index_reverse_fnc(number_of_cards, instruction) for instruction in shuffle_instructions]
        return _compose(reverse_functions)

    def _individual_index_reverse_fnc(self, number_of_cards : int, shuffle_instruction : ShuffleInstruction) -> Callable[[int],int]:
        (move, argument) = shuffle_instruction
        if move == ShuffleMove.Reverse:
            return self._reverse_reverse_fnc(number_of_cards)
        if move == ShuffleMove.Cut:
            return self._reverse_cut_fnc(number_of_cards, argument)
        if move == ShuffleMove.DealWithIncrement:
            return self._reverse_deal_with_increment_fnc(number_of_cards, argument)
        return lambda x : x

    def _reverse_reverse_fnc(self, number_of_cards : int) -> Callable[[int],int]:
        return functools.partial(self._reverse_reverse, number_of_cards)

    def _reverse_reverse(self, number_of_cards : int, index : int) -> int:
        return number_of_cards - 1 - index

    def _reverse_cut_fnc(self, number_of_cards : int, N : int) -> Callable[[int],int]:
        return functools.partial(self._reverse_cut, number_of_cards, N)

    def _reverse_cut(self, number_of_cards : int, N : int, index : int) -> int:
        cut_index = number_of_cards + N if N < 0 else N
        return index + cut_index if index < (number_of_cards - cut_index) else index - (number_of_cards - cut_index)

    def _reverse_deal_with_increment_fnc(self, number_of_cards : int, N : int) -> Callable[[int],int]:
        inverse_of_N = self._multiplicative_inverse(N, number_of_cards)
        return functools.partial(self._deal_with_increment, number_of_cards, inverse_of_N)

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

    def _deal_with_increment(self, number_of_cards : int, inverse_of_N : int, index : int) -> int:
        return (inverse_of_N * index) % number_of_cards



def main():
    start_time = time.time()
    shuffler = Shuffler()
    input_reader = ShuffleSequenceReader()
    input_file = "Advent20191222_1_input.txt"
    shuffle_instructions = input_reader.shuffle_instructions(input_file)
    reverse_fnc = shuffler.original_index_reverse_fnc(119315717514047, shuffle_instructions)
    index = 2020
    reporting_interval = 10000
    for n in range(101741582076661):
        index = reverse_fnc(index)
        if index == 2020:
            print("found repetition")
            print(n)
        if n > reporting_interval:
            reporting_interval = reporting_interval * 2
            print(n)
            print(time.time() - start_time)
            print(index)
    end_time = time.time()
    print(end_time - start_time)
    print(index)

def _compose(functions : List[Callable[[Any], Any]]) -> Callable[[Any], Any]:
    def compose2(f : Callable[[Any], Any], g : Callable[[Any], Any]):
        return lambda x : f(g(x))
    return functools.reduce(compose2, functions, lambda x: x)



if __name__ == "__main__":
    main()