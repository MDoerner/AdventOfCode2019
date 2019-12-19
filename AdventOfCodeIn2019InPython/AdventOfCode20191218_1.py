from __future__ import annotations
from enum import Enum, IntEnum, auto
from typing import List, Dict, Tuple, Any, IO, Optional, Set
import queue


Point = Tuple[int,int]

class Labyrinth:
    def __init__(self, labyrinth_rows : List[str]):
        self._corridors : Dict[Point, str] = {}
        self._entry_point = (-10,-10)
        self._keys : Dict[Point, str] = {}
        self._doors : Dict[Point, str] = {}
        self._load_labyrinth(labyrinth_rows)

    def _load_labyrinth(self, labyrinth_rows : List[str]):
        for y , row in enumerate(labyrinth_rows):
            for x , tile in enumerate(row):
                self._load_labyrinth_tile((x,y), tile)

    def _load_labyrinth_tile(self, point: Point, tile : str):
        if tile != '#':
            self._corridors[point] = tile
        if tile == '@':
            self._entry_point = point
        elif tile.isalpha():
            if tile.isupper():
                self._doors[point] = tile
            elif tile.islower():
                self._keys[point] = tile

    def is_walkable(self, point : Point) -> bool:
        return point in self._corridors

    def entry_point(self) -> Point:
        return self._entry_point

    def has_key(self, point : Point) -> bool:
        return point in self._keys

    def key(self, point : Point) -> Optional[str]:
        if not self.has_key(point):
            return None
        return self._keys[point]

    def has_door(self, point : Point) -> bool:
        return point in self._doors

    def required_key(self, point : Point) -> Optional[str]:
        if not self.has_door(point):
            return None
        door = self._doors[point]
        return door.lower()

    def number_of_keys(self) -> int:
        return len(self._keys)

    def show(self) -> str:
        (minimal_x, minimal_y, width, height) = self._dimensions()
        rows = [self._show_row(row_index, width, minimal_x, minimal_y) for row_index in range(height)]
        return "\n".join(rows)

    def _show_row(self, row_index : int, width : int, horizontal_offset : int, vertical_offset : int) -> str:
        display_items = [self._tile((column_index + horizontal_offset, row_index + vertical_offset)) for column_index in range(width)]
        character_representations = [self._character_representation(item) for item in display_items]
        return "".join(character_representations)

    def _tile(self, point : Point) -> str:
        return self._corridors.get(point, '#')

    def _character_representation(self, tile : str) -> str:
        if tile == '.':
            return " "
        if tile == '#':
            return "\u2588"
        return tile

    def _dimensions(self) -> Tuple[int, int, int, int]:
        non_empty_tiles = [point for point in self._corridors]
        x_coordinates = [x for (x,y) in non_empty_tiles]
        y_coordinates = [y for (x,y) in non_empty_tiles]
        minimal_x = min(x_coordinates) - 1
        width = max(x_coordinates) - minimal_x + 2
        minimal_y = min(y_coordinates) - 1
        height = max(y_coordinates) - minimal_y + 2
        return (minimal_x, minimal_y, width, height)


class LineReader:
    def read_input(self, filename : str) -> IO[str]:
        with open(filename, "r") as file:
            contents = file.read().splitlines()
        return contents


class PartialPath:
    def __init__(self, found_keys : Set[str], position : Point, path_length : int):
        self.found_keys = found_keys
        self.position = position
        self.path_length = path_length

    def extended_path(self, new_key : str, position : Point, path_length_to_key : int) -> PartialPath:
        return PartialPath(self.found_keys | frozenset(new_key), position, self.path_length + path_length_to_key)


class MazeSolver:
    def shortest_all_key_path_length(self, maze : Labyrinth) -> int:
        print(maze.number_of_keys())
        initial_path = PartialPath(frozenset(), maze.entry_point(), 0)
        current_key_count_paths = [initial_path]
        for key_count in range(maze.number_of_keys()):
            print(key_count)
            new_paths = {}
            for partial_path in current_key_count_paths:
                extended_paths = self.find_shortest_path_to_reachable_keys(maze, partial_path)
                for extended_path in extended_paths:
                    key = (extended_path.found_keys, extended_path.position)
                    if not key in new_paths \
                        or new_paths[key].path_length > extended_path.path_length:
                        new_paths[key] = extended_path
            current_key_count_paths = list(new_paths.values())
        return min([path.path_length for path in current_key_count_paths])

    def find_shortest_path_to_reachable_keys(self, maze : Labyrinth, partial_path : PartialPath) -> List[PartialPath]:
        collected_keys = partial_path.found_keys
        seen_keys = set(collected_keys)
        new_paths = []
        known_distances = {partial_path.position : 0}
        outermost_points = queue.Queue()
        outermost_points.put_nowait(partial_path.position)
        while not outermost_points.empty() and len(seen_keys) < maze.number_of_keys():
            current_position = outermost_points.get_nowait()
            for point in self._adjacent_points(current_position):
                if not point in known_distances and maze.is_walkable(point):
                    if not maze.has_door(point) or maze.required_key(point) in collected_keys:
                        known_distances[point] = known_distances[current_position] + 1
                        if maze.has_key(point) and not maze.key(point) in collected_keys:
                            key = maze.key(point)
                            new_partial_path = partial_path.extended_path(key, point, known_distances[point])
                            new_paths.append(new_partial_path)
                            seen_keys.add(key)
                        else:
                            outermost_points.put_nowait(point)
        return new_paths


    def _adjacent_points(self, point : Point) -> List[Point]:
        (x,y) = point
        return [(x-1,y), (x+1,y), (x,y-1), (x, y+1)]



def main():
    solver = MazeSolver()
    input_reader = LineReader()
    input_file = "Advent20191218_1_input.txt"
    labyrinth_map = input_reader.read_input(input_file)
    labyrinth = Labyrinth(labyrinth_map)
    shortest_path_length = solver.shortest_all_key_path_length(labyrinth)
    print(shortest_path_length)


if __name__ == "__main__":
    main()