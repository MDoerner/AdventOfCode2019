from __future__ import annotations
from enum import Enum, IntEnum, auto
from typing import List, Dict, Tuple, Any, IO, Optional, Set
import queue


Point = Tuple[int,int]
RecursivePoint = Tuple[Point,int] #The int is for the recursion level.

class RecursiveLabyrinth:
    def __init__(self, labyrinth_rows : List[str]):
        self._corridors : Dict[Point, str] = {}
        self._entry_points : List[RecursivePoint] = []
        self._keys : Dict[RecursivePoint, str] = {}
        self._doors : Dict[RecursivePoint, str] = {}
        self._portal_specifiers : Dict[Point, str] = {}
        self._portals : Dict[Point, Tuple[Point, int]] = {}
        self._load_labyrinth(labyrinth_rows)
        self._find_portals()

    def _load_labyrinth(self, labyrinth_rows : List[str]):
        for y , row in enumerate(labyrinth_rows):
            for x , tile in enumerate(row):
                self._load_labyrinth_tile((x,y), tile)

    def _load_labyrinth_tile(self, point: Point, tile : str):
        if tile == '.':
            self._corridors[point] = tile
        if tile.isalpha():
            self._portal_specifiers[point] = tile

    def _find_portals(self):
        portal_data = [data for data in [self._portal_data(point) for point in self._portal_specifiers] if not data is None]
        grouped_portal_data = self._grouped_portal_data(portal_data)
        for portal in grouped_portal_data:
            if portal == "AA":
                (point, level_delta) = grouped_portal_data[portal][0]
                self._entry_points.append((point, 0))
            elif portal == "ZZ":
                (point, level_delta) = grouped_portal_data[portal][0]
                self._keys[(point, 0)] = portal  # Actually the goal point. Allows to reuse the find all keys logic.
            else:
                (side1, level_jump1) = grouped_portal_data[portal][0]
                (side2, level_jump2) = grouped_portal_data[portal][1]
                self._portals[side1] = (side2, level_jump1)
                self._portals[side2] = (side1, level_jump2)

    def _portal_data(self, portal_point : Point) -> Optional[Tuple[str, Point, int]]:
        base_point = None
        other_portal_point = None
        for point in self._adjacent_points(portal_point):
            if point in self._portal_specifiers:
                other_portal_point = point
            elif self.is_walkable(point):
                base_point = point
        if base_point is None or other_portal_point is None:
            return None
        (x,y) = point
        level_jump = 1 if self._is_on_inside(portal_point) else -1
        portal_specifier = self._portal_specifier(((portal_point, self._portal_specifiers[portal_point]),(other_portal_point, self._portal_specifiers[other_portal_point])))
        return (portal_specifier, base_point, level_jump)

    def _adjacent_points(self, point : Point) -> List[Point]:
        (x,y) = point
        return [(x-1,y), (x+1,y), (x,y-1), (x, y+1)]

    def _portal_specifier(self, specifier_parts : Tuple[Tuple[Point, str], Tuple[Point, str]]) -> str:
        (((x1,y1), c1), ((x2,y2), c2)) = specifier_parts
        if x1 == x2:
            return c2 + c1 if y1 > y2 else c1 + c2
        else:
            return c2 + c1 if x1 > x2 else c1 + c2

    def _is_on_inside(self, point : Point) -> bool:
        (x_p, y_p) = point
        x_coordinates = [x for (x,y) in self._corridors]
        y_coordinates = [y for (x,y) in self._corridors]
        return min(x_coordinates) <= x_p <= max(x_coordinates) and min(y_coordinates) <= y_p <= max(y_coordinates)

    def _grouped_portal_data(self, portal_data : List[Tuple[str, Point, int]]) -> Dict[str, List[Tuple[Point,int]]]:
        grouped_data = {}
        for specifier, point, level_jump in portal_data:
            grouped_data.setdefault(specifier, []).append((point, level_jump))
        return grouped_data

    def is_walkable(self, point : Point) -> bool:
        return point in self._corridors

    def entry_points(self) -> List[RecursivePoint]:
        return self._entry_points

    def has_key(self, point : RecursivePoint) -> bool:
        return point in self._keys

    def key(self, point : RecursivePoint) -> Optional[str]:
        if not self.has_key(point):
            return None
        return self._keys[point]

    def has_door(self, point : RecursivePoint) -> bool:
        return point in self._doors

    def required_key(self, point : RecursivePoint) -> Optional[str]:
        if not self.has_door(point):
            return None
        door = self._doors[point]
        return door.lower()

    def number_of_keys(self) -> int:
        return len(self._keys)

    def has_portal(self, recursive_point : RecursivePoint) -> bool:
        (point, level) = recursive_point
        if level > 0:
            return point in self._portals
        if point in self._portals:
            (target_point, level_change) = self._portals[point]
            return level_change >= 0
        return False

    def portal_destination(self, recursive_point : RecursivePoint) -> Optional[RecursivePoint]:
        (point, level) = recursive_point
        if point in self._portals:
            (target_point, level_change) = self._portals[point]
            target_level = level + level_change
            if target_level >= 0:
                return (target_point, target_level)
        return None

    def show(self) -> str:
        (minimal_x, minimal_y, width, height) = self._dimensions()
        rows = [self._show_row(row_index, width, minimal_x, minimal_y) for row_index in range(height)]
        return "\n".join(rows)

    def _show_row(self, row_index : int, width : int, horizontal_offset : int, vertical_offset : int) -> str:
        display_items = [self._tile((column_index + horizontal_offset, row_index + vertical_offset)) for column_index in range(width)]
        character_representations = [self._character_representation(item) for item in display_items]
        return "".join(character_representations)

    def _tile(self, point : Point) -> str:
        if point in self._portal_specifiers:
            return self._portal_specifiers[point]
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
        minimal_x = min(x_coordinates) - 2
        width = max(x_coordinates) - minimal_x + 3
        minimal_y = min(y_coordinates) - 2
        height = max(y_coordinates) - minimal_y + 3
        return (minimal_x, minimal_y, width, height)


class LineReader:
    def read_input(self, filename : str) -> IO[str]:
        with open(filename, "r") as file:
            contents = file.read().splitlines()
        return contents


class PartialPath:
    def __init__(self, found_keys : Set[str], positions : Tuple[RecursivePoint], path_length : int):
        self.found_keys = found_keys
        self.positions = positions
        self.path_length = path_length

    def extended_path(self, new_key : str, index : int, position : RecursivePoint, path_length_to_key : int) -> PartialPath:
        return PartialPath(self.found_keys | frozenset(new_key), self._replace_index(self.positions, index, position), self.path_length + path_length_to_key)

    def _replace_index(self, old_tuple : tuple, index : int, new_item: Any) -> tuple:
        return tuple([new_item if index == current_index else current_item for current_index, current_item in enumerate(old_tuple)])


class PortalMazeSolver:
    def shortest_all_key_path_length(self, maze : RecursiveLabyrinth) -> int:
        print(maze.number_of_keys())
        initial_path = PartialPath(frozenset(), tuple(maze.entry_points()), 0)
        current_key_count_paths = [initial_path]
        for key_count in range(maze.number_of_keys()):
            print(key_count)
            new_paths = {}
            for partial_path in current_key_count_paths:
                for index in range(len(partial_path.positions)):
                    extended_paths = self.find_shortest_path_to_reachable_keys(maze, partial_path, index)
                    for extended_path in extended_paths:
                        key = (extended_path.found_keys, extended_path.positions)
                        if not key in new_paths \
                            or new_paths[key].path_length > extended_path.path_length:
                            new_paths[key] = extended_path
            current_key_count_paths = list(new_paths.values())
        return min([path.path_length for path in current_key_count_paths])

    def find_shortest_path_to_reachable_keys(self, maze : RecursiveLabyrinth, partial_path : PartialPath, index : int) -> List[PartialPath]:
        collected_keys = partial_path.found_keys
        seen_keys = set(collected_keys)
        new_paths = []
        known_distances = {partial_path.positions[index] : 0}
        outermost_points = queue.Queue()
        outermost_points.put_nowait(partial_path.positions[index])
        while not outermost_points.empty() and len(seen_keys) < maze.number_of_keys():
            current_recursive_position = outermost_points.get_nowait()
            (current_position, current_level) = current_recursive_position
            points_to_move_to = [(point, current_level) for point in self._adjacent_points(current_position)]
            if maze.has_portal(current_recursive_position):
                points_to_move_to.append(maze.portal_destination(current_recursive_position))
                print(current_recursive_position)
                print(maze.portal_destination(current_recursive_position))
            for recursive_point in points_to_move_to:
                (point, level) = recursive_point
                if not recursive_point in known_distances and maze.is_walkable(point):
                    if not maze.has_door(recursive_point) or maze.required_key(recursive_point) in collected_keys:
                        known_distances[recursive_point] = known_distances[current_recursive_position] + 1
                        if maze.has_key(recursive_point) and not maze.key(recursive_point) in collected_keys:
                            key = maze.key(recursive_point)
                            new_partial_path = partial_path.extended_path(key, index, recursive_point, known_distances[recursive_point])
                            new_paths.append(new_partial_path)
                            seen_keys.add(key)
                        else:
                            outermost_points.put_nowait(recursive_point)
        return new_paths

    def _adjacent_points(self, point : Point) -> List[Point]:
        (x,y) = point
        return [(x-1,y), (x+1,y), (x,y-1), (x, y+1)]



def main():
    solver = PortalMazeSolver()
    input_reader = LineReader()
    input_file = "Advent20191220_1_input.txt"
    labyrinth_map = input_reader.read_input(input_file)
    labyrinth = RecursiveLabyrinth(labyrinth_map)
    shortest_path_length = solver.shortest_all_key_path_length(labyrinth)
    print(shortest_path_length)



if __name__ == "__main__":
    main()