from typing import List, Dict, Tuple, Any, IO, Optional, Callable

class ImageLayer:
    def __init__(self, width : int, pixel_stream : List[int]):
        self.pixels = self._image_from_stream(width, pixel_stream)
        
    def _image_from_stream(self, width : int, pixel_stream : List[int]) -> List[List[int]]:
        rows = []
        for first_index_of_row in range(0, len(pixel_stream), width):
            rows.append(pixel_stream[first_index_of_row:(first_index_of_row + width)])
        return rows

    def color_count(self, color : int) -> int:
        return self._flatten_list_of_lists(self.pixels).count(color)

    def _flatten_list_of_lists(self, lists : List[List[Any]]) -> List[Any]:
        return [item for lst in lists for item in lst]
        

class SpaceImage:
    def __init__(self, width : int, height : int, pixel_stream : List[int]):
        self.layers = self._layer_from_stream(width, height * width, pixel_stream)
        self.width = width
        self.height = height

    def _layer_from_stream(self, width : int, pixels_per_layer, pixel_stream : List[int]) -> List[ImageLayer]:
        layers = []
        for first_index_of_layer in range(0, len(pixel_stream), pixels_per_layer):
            layer_pixels = pixel_stream[first_index_of_layer:(first_index_of_layer + pixels_per_layer)]
            layers.append(ImageLayer(width, layer_pixels))
        return layers

    def with_min_color(self, color : int) -> ImageLayer:
        return _with_min_function_result(self.layers, lambda layer: layer.color_count(color))


def main():
    pixel_stream = _read_pixel_stream("Advent20191208_1_input.txt")
    space_image = SpaceImage(25, 6, pixel_stream)
    fewest_zero_layer = space_image.with_min_color(0)
    check_sum = fewest_zero_layer.color_count(1) * fewest_zero_layer.color_count(2)
    print(check_sum)

def _read_pixel_stream(filename : str) -> IO[List[int]]:
    return _to_digits(_read_input(filename))

def _to_digits(digit_string: str) -> List[int]:
    return [int(d) for d in digit_string]

def _read_input(filename : str) -> IO[str]:
    with open(filename, "r") as file:
        contents = file.read()
    return contents

def _with_min_function_result(lst : List[Any], func : Callable[[Any], Any]) -> Any:
    if not lst:
        return None
    min_element = lst[0]
    min_value = func(min_element)
    for item in lst:
        value = func(item)
        if value < min_value:
            min_value = value
            min_element = item
    return min_element


if __name__ == "__main__":
    main()