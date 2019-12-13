from __future__ import annotations
from typing import List, Dict, Tuple, Any, IO, Optional
import re


Point = Tuple[int, int, int]

class Vector:
    def __init__(self, point : Point):
        self.vector = point

    def add(self, other_vector : Vector) -> Vector:
        return Vector(other_vector.move_point(self.vector))
    
    def move_point(self, point : Point) -> Point:
        components = [a + b for (a, b) in zip(point, self.vector)]
        return tuple(components)

    def subtract(self, other_vector : Vector) -> Vector:
        return self.add(other_vector.negate())

    def negate(self) -> Vector:
        components = [-a for a in self.vector]
        return Vector(tuple(components))

    def signs(self) -> Vector:
        components = [self._sign(a) for a in self.vector]
        return Vector(tuple(components))

    def _sign(self, number : int) -> int:
        return 0 if number == 0 else -1 if number < 0 else 1 


class Body:
    def __init__(self, position : Point, velocity : Vector):
        self.position = position
        self.velocity = velocity

    def potential_energy(self) -> int:
        return sum([abs(a) for a in self.position])

    def kinetic_energy(self) -> int:
        return sum([abs(a) for a in self.velocity.vector])

    def energy(self) -> int:
        return self.kinetic_energy() * self.potential_energy()

    def accelerate(self, additional_velocity : Vector):
        self.velocity = self.velocity.add(additional_velocity)

    def move(self, displacement : Vector):
        self.position = displacement.move_point(self.position)

    def displacement_from(self, other_body : Body) -> Vector:
        return self._difference_vector(other_body.position, self.position)

    def _difference_vector(self, start_point : Point, end_point : Point) -> Vector:
        components = [b - a for (a, b) in zip(start_point, end_point)]
        return Vector(tuple(components))


class NBodySystem:
    def __init__(self, bodies : List[Body]):
        self.bodies = bodies

    def total_energy(self):
        return sum([body.energy() for body in self.bodies])

    def step(self, steps : int):
        if steps <= 0:
            return None
        for n in range(steps):
            self._apply_single_step()

    def _apply_single_step(self):
        self._apply_gravity()
        self._apply_velocity()

    def _apply_gravity(self):
        if len(self.bodies) < 2:
            return None
        for first_body_index in range(len(self.bodies) - 1):
            first_body = self.bodies[first_body_index]
            for second_body in self.bodies[(first_body_index+1):]:
                self._apply_gravity_to_pair(first_body, second_body)    

    def _apply_gravity_to_pair(self, body1 : Body, body2 : Body):
        gravity_pull = body2.displacement_from(body1).signs()
        body1.accelerate(gravity_pull)
        body2.accelerate(gravity_pull.negate())

    def _apply_velocity(self):
        for body in self.bodies:
            self._apply_velocity_individually(body)

    def _apply_velocity_individually(self, body : Body):
        body.move(body.velocity)
        
class PositionReader:
    _position_pattern = r"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>"
    
    def positions(self, filename : str) -> IO[List[Point]]:
        input_lines = self._read_input(filename)
        return [self._to_position(line) for line in input_lines]
    
    def _read_input(self, filename : str) -> IO[List[str]]:
        with open(filename, "r") as file:
            contents = file.readlines()
        return contents

    def _to_position(self, specification : str) -> Point:
        coordinate_matches = re.match(self._position_pattern, specification)
        coordinates = [int(n) for n in coordinate_matches.group(1, 2, 3)]
        return tuple(coordinates)
        


def main():
    input_reader = PositionReader()
    input_file = "Advent20191212_1_input.txt"
    start_positions = input_reader.positions(input_file)
    start_velocities = [Vector((0, 0, 0)) for n in range(len(start_positions))]
    bodies = [Body(position, velocity) for position, velocity in zip(start_positions, start_velocities)]
    n_body_system = NBodySystem(bodies)
    n_body_system.step(1000)
    print(n_body_system.total_energy())


if __name__ == "__main__":
    main()