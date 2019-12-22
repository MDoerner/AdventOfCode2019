from __future__ import annotations
from enum import Enum, IntEnum, auto
from typing import List, Dict, Tuple, Any, IO, Optional, Set
import re
import operator

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
        current_deck = deck
        for instruction in shuffle_instructions:
            current_deck = self._apply_instruction(current_deck, instruction)
        return current_deck

    def _apply_instruction(self, deck : CardDeck, shuffle_instruction : ShuffleInstruction) -> CardDeck:
        (move, argument) = shuffle_instruction
        if move == ShuffleMove.Reverse:
            return self._reverse(deck)
        if move == ShuffleMove.Cut:
            return self._cut(deck, argument)
        if move == ShuffleMove.DealWithIncrement:
            return self._deal_with_increment(deck, argument)
        return deck

    def _reverse(self, deck : CardDeck) -> CardDeck:
        return list(reversed(deck))

    def _cut(self, deck : CardDeck, N : int) -> CardDeck:
        return deck[N:] + deck[:N]

    def _deal_with_increment(self, deck : CardDeck, N : int) -> CardDeck:
        number_of_cards = len(deck)
        cards_with_new_index = [(((N * index) % number_of_cards), card) for index, card in enumerate(deck)]
        cards_with_new_index.sort(key = operator.itemgetter(0))
        return [card for index, card in cards_with_new_index]



def main():
    shuffler = Shuffler()
    input_reader = ShuffleSequenceReader()
    input_file = "Advent20191222_1_input.txt"
    shuffle_instructions = input_reader.shuffle_instructions(input_file)
    deck = list(range(10007))
    shuffled_deck = shuffler.shuffle(deck, shuffle_instructions)
    requested_index = shuffled_deck.index(2019)
    print(requested_index)



if __name__ == "__main__":
    main()