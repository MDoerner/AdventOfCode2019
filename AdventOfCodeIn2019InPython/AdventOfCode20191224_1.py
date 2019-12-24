from __future__ import annotations
from enum import Enum, IntEnum, auto
from typing import List, Dict, Tuple, Any, IO, Optional

class Bug(Enum):
    Empty = 0
    Bug = 1


class BugGridReader:
    def bug_grid(self, filename : str) -> IO[Dict[Point, Bug]]:
        input_string = self._read_input(filename)
        return self.to_bug_grid(input_string)

    def _read_input(self, filename : str) -> IO[str]:
        with open(filename, "r") as file:
            contents = file.read()
        return contents

    def to_bug_grid(self, input_string : str) -> Dict[Point, Bug]:
        def to_bug(c : str) -> Bug:
            if c == '.':
                return Bug.Empty
            if c == '#':
                return Bug.Bug
            return None

        grid = {}
        lines = input_string.splitlines()
        for y, line in enumerate(lines):
            for x, c in enumerate(line):
                grid[(x,y)] = to_bug(c)
        return grid


Point = Tuple[int,int]

class GameOfLife:
    def __init__(self, grid : Dict[Point, Bug]):
        self._grid = grid
        self._set_grid_width()

    def _set_grid_width(self):
        x_coordinates = [x for (x,y) in self._grid]
        self._grid_width = max(x_coordinates) - min(x_coordinates) + 1

    def copy(self) -> GameOfLife:
        return GameOfLife(self._grid.copy(), self._grid_width)

    def bug(self, point : Point) -> Bug:
        return self._grid.get(point, Bug.Empty)

    def biodiversity(self):
        return sum([2 ** self._grid_index(point) for point, bug in self._grid.items() if bug != Bug.Empty])

    def _grid_index(self, point : Point) -> int:
        (x,y) = point
        return y * self._grid_width + x

    def step_once(self):
        self._grid = {point : self._live_or_die(point) for point in self._grid}

    def _live_or_die(self, point : Point) -> Bug:
        def adjacent_points(point : Point) -> List[Point]:
            (x,y) = point
            return [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
        bug_state = self.bug(point)
        adjacent_bug_states = [self.bug(adjacent_point) for adjacent_point in adjacent_points(point)]
        number_of_adjacent_bugs = len([bug_state for bug_state in adjacent_bug_states if bug_state != Bug.Empty])
        if bug_state == Bug.Bug and number_of_adjacent_bugs != 1:
            return Bug.Empty
        if bug_state == Bug.Empty and 1 <= number_of_adjacent_bugs <= 2:
            return Bug.Bug
        return bug_state

    def show(self) -> str:
        def character_representation(bug : Bug) -> str:
            if bug == Bug.Empty:
                return '.'
            if bug == Bug.Bug:
                return '#'
            return None

        grid_lines = []
        y = 0
        while (0,y) in self._grid:
            grid_line = "".join([character_representation(self.bug((x,y))) for x in range(self._grid_width)])
            grid_lines.append(grid_line)
            y = y + 1
        return "\n".join(grid_lines)




def main():
    input_reader = BugGridReader()
    input_file = "Advent20191224_1_input.txt"
    grid = input_reader.bug_grid(input_file)
    game_of_life = GameOfLife(grid)
    current_biodiversity = game_of_life.biodiversity()
    encountered_biodiversities = set()
    while not current_biodiversity in encountered_biodiversities:
        encountered_biodiversities.add(current_biodiversity)
        game_of_life.step_once()
        current_biodiversity = game_of_life.biodiversity()
    print(current_biodiversity)



if __name__ == "__main__":
    main()

