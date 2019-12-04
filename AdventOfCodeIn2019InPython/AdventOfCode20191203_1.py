def main():
    input_lines = read_input("Advent20191203_1_input.txt")
    relative_wires = [relative_sections(x) for x in input_lines]
    absolute_wires = [absolute_sections((0, 0), wire) for wire in relative_wires]
    intersections = wire_intersections(absolute_wires[0], absolute_wires[1])
    intersections_distances = [manhatten_distance((0, 0), intersection) for intersection in intersections if intersection != (0, 0)]
    minimal_distance = min(intersections_distances)
    print(minimal_distance)

def read_input(filename):
    with open(filename, "r") as file:
        contents = file.readlines()
    return contents

def relative_sections(input_string):
    return [(x[0], int(x[1:])) for x in input_string.split(',')]

def absolute_sections(start_point, rel_sections):
    sections = []
    section_start = start_point
    for section in rel_sections:
        section_end = end_point(section_start, section)
        sections.append((section_start, section_end))
        section_start = section_end
    return sections

def end_point(start_point, relative_section):
    (x, y) = start_point
    (direction, distance) = relative_section
    if direction == 'L':
        return (x - distance, y) 
    elif direction == 'R':
        return (x + distance, y)
    elif direction == 'U':
        return (x, y + distance)
    elif direction == 'D':
        return (x, y - distance)
    raise ValueError("Not supported direction", direction)

def wire_intersections(absolute_wire1, absolute_wire2):
    horizontal_sections1 = [section for section in absolute_wire1 if is_horizontal(section)]
    horizontal_sections2 = [section for section in absolute_wire2 if is_horizontal(section)]
    vertical_sections1 = [section for section in absolute_wire1 if not is_horizontal(section)]
    vertical_sections2 = [section for section in absolute_wire2 if not is_horizontal(section)]
    intersections = section_intersections(horizontal_sections1, vertical_sections2) + section_intersections(horizontal_sections2, vertical_sections1)
    return intersections

def is_horizontal(section):
    (start_point, end_point) = section
    (start_x, start_y) = start_point
    (end_x, end_y) = end_point
    return start_x != end_x

# There is a potentially more efficient method by first sorting. 
# However, the simple approach is used out of lazyness.
def section_intersections(horizontal_sections, vertical_sections):
    intersections = []
    for horizontal_section in horizontal_sections:
        for vertical_section in vertical_sections:
            intersection = section_intersection(horizontal_section, vertical_section)
            if intersection != None:
                intersections.append(intersection)
    return intersections

def section_intersection(horizontal_section, vertical_section):
    ((x_1_h, y_1_h), (x_2_h, y_2_h)) = horizontal_section
    ((x_1_v, y_1_v), (x_2_v, y_2_v)) = vertical_section
    if (x_1_h <= x_1_v <= x_2_h or x_1_h >= x_1_v >= x_2_h) and (y_1_v <= y_1_h <= y_2_v or y_1_v >= y_1_h >= y_2_v):
        return (x_1_v, y_1_h)
    return None

def manhatten_distance(point1, point2):
    (x1, y1) = point1
    (x2, y2) = point2
    return abs(x1 - x2) + abs(y1 - y2)


if __name__ == "__main__":
    main()