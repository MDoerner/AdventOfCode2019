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
                grid[(x,y,0)] = to_bug(c)
        return grid


Point = Tuple[int,int,int]

class GameOfLife:
    def __init__(self, grid : Dict[Point, Bug]):
        self._grid = grid
        self._set_grid_width()
        self._set_grid_height()
        self._clear_center()
        self._purge_empty_grid_nodes()

    def _set_grid_width(self):
        x_coordinates = [x for (x,y,z) in self._grid]
        self._grid_width = max(x_coordinates) - min(x_coordinates) + 1

    def _set_grid_height(self):
        y_coordinates = [y for (x,y,z) in self._grid]
        self._grid_height = max(y_coordinates) - min(y_coordinates) + 1

    def _clear_center(self):
        del self._grid[(self._grid_width // 2, self._grid_height // 2, 0)]

    def _purge_empty_grid_nodes(self):
        self._grid = {point : self._grid[point] for point in self._grid if self._grid[point] != Bug.Empty}

    def copy(self) -> GameOfLife:
        return GameOfLife(self._grid.copy())

    def bug(self, point : Point) -> Bug:
        return self._grid.get(point, Bug.Empty)

    def biodiversity(self, level : int):
        return sum([2 ** self._grid_index((x,y,z)) for (x,y,z), bug in self._grid.items() if bug != Bug.Empty and z == level])

    def _grid_index(self, point : Point) -> int:
        (x,y,z) = point
        return y * self._grid_width + x

    def step_once(self):
        points_to_consider = list(self._grid) + [point for grid_point in self._grid for point in self._adjacent_points(grid_point)]
        self._grid = {point : self._live_or_die(point) for point in points_to_consider}
        self._purge_empty_grid_nodes()

    def _adjacent_points(self, point : Point) -> List[Point]:
        def points_to_the_left(grid_point : Point) -> List[Point]:
            (x,y,z) = grid_point
            if x == 0:
                return [(self._grid_width // 2 - 1, self._grid_height // 2, z - 1)]
            if x == self._grid_width // 2 + 1 and y == self._grid_height // 2:
                return [(self._grid_width - 1, new_y, z + 1) for new_y in range(self._grid_height)]
            return [(x - 1, y, z)]

        def points_above(grid_point : Point) -> List[Point]:
            (x,y,z) = grid_point
            if y == 0:
                return [(self._grid_width // 2, self._grid_height // 2 - 1, z - 1)]
            if y == self._grid_height // 2 + 1 and x == self._grid_width // 2:
                return [(new_x, self._grid_height - 1, z + 1) for new_x in range(self._grid_width)]
            return[(x, y - 1, z)]

        def points_to_the_right(grid_point : Point) -> List[Point]:
            (x,y,z) = grid_point
            if x == self._grid_width - 1:
                return [(self._grid_width // 2 + 1, self._grid_height // 2, z - 1)]
            if x == self._grid_width // 2 - 1 and y == self._grid_height // 2:
                return [(0, new_y, z + 1) for new_y in range(self._grid_height)]
            return [(x + 1, y, z)]

        def points_below(grid_point : Point) -> List[Point]:
            (x,y,z) = grid_point
            if y == self._grid_height - 1:
                return [(self._grid_width // 2, self._grid_height // 2 + 1, z - 1)]
            if y == self._grid_height // 2 - 1 and x == self._grid_width // 2:
                return [(new_x, 0, z + 1) for new_x in range(self._grid_width)]
            return [(x, y + 1, z)]

        return points_to_the_left(point) + points_above(point) + points_to_the_right(point) + points_below(point)

    def _live_or_die(self, point : Point) -> Bug:
        bug_state = self.bug(point)
        adjacent_bug_states = [self.bug(adjacent_point) for adjacent_point in self._adjacent_points(point)]
        number_of_adjacent_bugs = len([bug_state for bug_state in adjacent_bug_states if bug_state != Bug.Empty])
        if bug_state == Bug.Bug and number_of_adjacent_bugs != 1:
            return Bug.Empty
        if bug_state == Bug.Empty and 1 <= number_of_adjacent_bugs <= 2:
            return Bug.Bug
        return bug_state

    def number_of_bugs(self) -> int:
        return len(self._grid)

    def show(self, level : int) -> str:
        def character_representation(bug : Bug) -> str:
            if bug == Bug.Empty:
                return '.'
            if bug == Bug.Bug:
                return '#'
            return None

        grid_lines = []
        for y in range(self._grid_height):
            grid_line = "".join([character_representation(self.bug((x,y,level))) for x in range(self._grid_width)])
            grid_lines.append(grid_line)
        return "\n".join(grid_lines)



def main():
    input_reader = BugGridReader()
    input_file = "Advent20191224_1_input.txt"
    grid = input_reader.bug_grid(input_file)
    game_of_life = GameOfLife(grid)
    for n in range(200):
        game_of_life.step_once()
    print(game_of_life.number_of_bugs())



if __name__ == "__main__":
    main()

