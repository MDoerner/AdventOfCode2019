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
    (most_asteroids_visible_from_asteroid, num_asteroids) = _maximize_asteroid_direction(asteroids)
    other_asteroids = [asteroid for asteroid in asteroids if asteroid != most_asteroids_visible_from_asteroid]
    asteroid_destruction_order = _destruction_order(most_asteroids_visible_from_asteroid, other_asteroids)
    print(asteroid_destruction_order[199])


def _destruction_order(base_asteroid : Point, other_asteroids : List[Point]) -> List[Point]:
    by_direction = _by_realtive_direction(base_asteroid, other_asteroids)
    ordered_directions = sorted(by_direction.keys(), key=_direction_order)
    destroyed_asteroids = []
    while by_direction:
        for direction in ordered_directions:
            if direction in by_direction:
                asteroids_in_direction = by_direction[direction]
                destroyed_asteroids.append(asteroids_in_direction.pop())
                if not asteroids_in_direction:
                    del by_direction[direction]
    return destroyed_asteroids

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
     
def _direction_order(direction: Direction) -> float:
    if direction == (0,0):
        return -1
    (x, y) = direction
    if x >= 0: 
        return y + 1
    return 3 - y

def _by_realtive_direction(base_point : Point, other_points : List[Point]) -> Dict[Direction, List[Point]]:
    by_direction : Dict[Direction, List[Point]] = {}
    for other_point in other_points:
        direction = _to_direction(_difference_vector(base_point, other_point))
        by_direction.setdefault(direction, []).append(other_point)
    for value_list in by_direction.values():
        value_list.sort(key= lambda x : _l1_norm(_difference_vector(base_point, x)), reverse=True)
    return by_direction


if __name__ == "__main__":
    main()