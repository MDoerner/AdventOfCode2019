from enum import Enum, IntEnum, auto
from itertools import permutations
from typing import List, Dict, Tuple, Any, IO, Optional, Callable
import queue

Point = Tuple[int, int]
Vector = Tuple[int, int]
Direction = Tuple[float, float]



class AsteroidePositionReader:
    def asteroid_positions(self, filename : str) -> IO[List[Point]]:
        input_string = self._read_input(filename)
        return self.to_asteroid_positions(input_string)

    def _read_input(self, filename : str) -> IO[str]:
        with open(filename, "r") as file:
            contents = file.read()
        return contents

    def to_asteroid_positions(self, map : str) -> List[Point]:
        lines = map.splitlines()
        return [(x,y) for (y, row) in enumerate(lines) for (x, value) in enumerate(row) if value == '#']



def main():
    input_reader = AsteroidePositionReader()
    input_file = "Advent20191210_1_input.txt"
    asteroids = input_reader.asteroid_positions(input_file)
    most_asteroids_visible_from_asteroid = _maximize_asteroid_direction(asteroids)
    print(most_asteroids_visible_from_asteroid)

def _maximize_asteroid_direction(asteroids : List[Point]) -> Tuple[Point, int]:
    return _maximizer(asteroids, _asteroid_direction_counter(asteroids))

def _maximizer(items : List[Any], func : Callable[[Any], Any]) -> Optional[Tuple[Any, Any]]:
    if not items:
        return None
    maximizer = items[0]
    maximum = func(maximizer)
    for item in items:
        value = func(item)
        if value > maximum:
            maximum = value
            maximizer = item
    return (maximizer, maximum)

def _asteroid_direction_counter(asteroids : List[Point]) -> Callable[[Point], int]:
    return lambda x : _asteroid_direction_count(x, asteroids)

def _asteroid_direction_count(base_point : Point, asteroids : List[Point]) -> int:
    directions = [_to_direction(_difference_vector(base_point, asteroid)) for asteroid in asteroids if asteroid != base_point]
    return len(set(directions))

def _difference_vector(base_point : Point, point : Point) -> Vector:
    (x_b, y_b) = base_point
    (x, y) = point
    return (x - x_b, y - y_b)

def _l1_norm(vector : Vector) -> int:
    (x, y) = vector
    return abs(x) + abs(y)

def _to_direction(vector : Vector) -> Direction:
    if vector == (0, 0):
        return vector
    norm = _l1_norm(vector)
    (x, y) = vector
    return (x/norm, y/norm)


if __name__ == "__main__":
    main()