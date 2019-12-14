from __future__ import annotations
from enum import Enum, IntEnum, auto
from typing import List, Dict, Tuple, Any, IO, Optional, Callable
import re


class Recipe:
    def __init__(self, name : str, quantity : int, ingrediends : List[Tuple[str, int]]):
        self.product = name
        self.produced_quantity = quantity
        self.ingredients = ingrediends

    def __str__(self):
        return f"{str(self.produced_quantity)} {str(self.product)} <= {str(self.ingredients)}"


class RecipeReader:
    _input_pattern = r"(.+) => (.+)"
    _element_pattern = r"\s*(\d+)\s+(\w+)\s*"
    
    def recipes(self, filename : str) -> IO[Dict[str, Recipe]]:
        input_strings = self._read_input(filename)
        return self.to_recipes(input_strings)

    def _read_input(self, filename : str) -> IO[List[str]]:
        with open(filename, "r") as file:
            contents = file.readlines()
        return contents

    def to_recipes(self, input_strings : List[str]) -> Dict[str, Recipe]:
        recipes = [self.to_recipe(input_string) for input_string in input_strings]
        return {recipe.product: recipe for recipe in recipes}

    def to_recipe(self, recipe_string : str) -> Recipe:
        main_match = re.match(self._input_pattern, recipe_string)
        (ingredient_list_string, product_string) = main_match.group(1, 2)
        ingredient_list = [self.to_ingredient(ingredient_string) for ingredient_string in ingredient_list_string.split(',')]
        (product_name, product_quantity) = self.to_ingredient(product_string)
        return Recipe(product_name, product_quantity, ingredient_list)

    def to_ingredient(self, ingredient_string : str) -> Tuple[str, int]:
        ingredient_match = re.match(self._element_pattern, ingredient_string)
        (quantity, name) = ingredient_match.group(1, 2)
        return (name, int(quantity))


class NanoFactory:
    def __init__(self, recipes : Dict[str, Recipe]):
        self._recipes = recipes
        self._sorted_ingredients = self._topological_sort(recipes)
        self._ingredient_orders = self._to_order_dictionary(self._sorted_ingredients)

    def _topological_sort(self, recipes : Dict[str, Recipe]) -> List[str]:
        vertices = set(list(recipes.keys()) + [ingredient for recipe in recipes.values() for (ingredient, quantity) in recipe.ingredients])
        ingoing_edges = {vertex : [] for vertex in vertices}
        for recipe in recipes.values():
            for (ingredient, quantity) in recipe.ingredients:
                ingoing_edges[ingredient].append(recipe.product)
        vertices_wo_inflow = [vertex for vertex in ingoing_edges if not ingoing_edges[vertex]]
        sorted_vertices = []
        while vertices_wo_inflow:
            next_vertex = vertices_wo_inflow[0]
            sorted_vertices.append(next_vertex)
            vertices_wo_inflow.remove(next_vertex)
            outgoing_edges = recipes.get(next_vertex, Recipe(next_vertex, 0, [])).ingredients
            for (vertex, quantity) in outgoing_edges:
                ingoing = ingoing_edges[vertex]
                ingoing.remove(next_vertex)
                if not ingoing:
                    vertices_wo_inflow.append(vertex)
        return sorted_vertices

    def _to_order_dictionary(self, items : List[str]) -> Dict[str, int]:
        return {item : index for index, item in enumerate(items)}

    def required_base_ingredients(self, required_ingredients : Dict[str,int]) -> Dict[str,int]:
        if not required_ingredients:
            return {}
        ingredients = required_ingredients.copy()
        non_processed_non_base_ingredients = [ingredient for ingredient in ingredients if ingredient in self._recipes]
        while non_processed_non_base_ingredients:
            highest_level_ingredient = self._highest_level_ingredient(non_processed_non_base_ingredients)
            recipe = self._recipes[highest_level_ingredient]
            required_quantity = ingredients[highest_level_ingredient]
            new_ingredients = self._required_ingredients(recipe, required_quantity)
            for (new_ingredient, quantity) in new_ingredients:
                ingredients[new_ingredient] = ingredients.get(new_ingredient, 0) + quantity
            del ingredients[highest_level_ingredient]
            non_processed_non_base_ingredients = [ingredient for ingredient in ingredients if ingredient in self._recipes]
        return ingredients

    def _highest_level_ingredient(self, ingredients : List[str]) -> str:
        (ingredient, level) = self._minimizer(ingredients, lambda x: self._ingredient_orders[x])
        return ingredient

    def _minimizer(self, items : List[Any], func : Callable[[Any], Any]) -> Optional[Tuple[Any, Any]]:
        if not items:
            return None
        minimizer = items[0]
        minimum = func(minimizer)
        for item in items:
            value = func(item)
            if value < minimum:
                minimum = value
                minimizer = item
        return (minimizer, minimum)
            
    def _required_ingredients(self, recipe : Recipe, required_quantity: int) -> List[Tuple[str,int]]:
        batch_size = recipe.produced_quantity
        number_of_required_batches = self._smallest_larger_multiple(required_quantity, batch_size)
        return [(name, number_of_required_batches * quantity) for name, quantity in recipe.ingredients]

    def _smallest_larger_multiple(self, quantity : int, batch_size : int) -> int:
        if quantity % batch_size == 0:
            return quantity // batch_size
        return (quantity // batch_size) + 1

    def quantity_possible_to_produce(self, product : str, base_ingredients : Dict[str,int]) -> int:
        if not self._all_required_ingredients_present(product, base_ingredients):
            return 0
        lower_bound = self._lower_bound_for_producible_amount(product, base_ingredients)
        (lower_bound, upper_bound) = self._scale_to_find_upper_bound(lower_bound, product, base_ingredients)
        return self._binary_search_quantity(lower_bound, upper_bound, product, base_ingredients)

    def _all_required_ingredients_present(self, product : str, base_ingredients : Dict[str,int]) -> bool:
        for_one = self.required_base_ingredients({product: 1})
        for required_base_ingredient in for_one:
            if not required_base_ingredient in base_ingredients:
                return False
        return True
         
    def _lower_bound_for_producible_amount(self, product : str, base_ingredients : Dict[str,int]) -> int:
        for_one = self.required_base_ingredients({product: 1})
        return min([base_ingredients[ingredient] // for_one[ingredient] for ingredient in for_one])

    def _scale_to_find_upper_bound(self, initial_lower_bound : int, product : str, base_ingredients : Dict[str,int]) -> Tuple[int,int]:
        '''Returns (lower_bound, upper_bound).'''
        lower_bound = initial_lower_bound
        upper_bound = 2 * lower_bound if lower_bound > 0 else 1
        for_upper_bound = self.required_base_ingredients({product: upper_bound})
        while self._has_sufficient_base_material(for_upper_bound, base_ingredients):
            lower_bound = upper_bound
            upper_bound = upper_bound * 2
            for_upper_bound = self.required_base_ingredients({product: upper_bound})
        return (lower_bound, upper_bound)

    def _has_sufficient_base_material(self, requires_materials : Dict[str, int], present_materials : Dict[str, int]) -> bool:
        return all([(present_materials[ingredient] >= requires_materials[ingredient]) for ingredient in requires_materials])

    def _binary_search_quantity(self, lower_bound : int, upper_bound : int, product : str, base_ingredients : Dict[str,int]) -> Tuple[int,int]:
        test_quantity = (lower_bound + upper_bound) // 2
        for_test_quantity = self.required_base_ingredients({product: test_quantity})        
        if not self._has_sufficient_base_material(for_test_quantity, base_ingredients):
            return self._binary_search_quantity(lower_bound, test_quantity - 1, product, base_ingredients)
        for_one_more = self.required_base_ingredients({product: test_quantity + 1})       
        if self._has_sufficient_base_material(for_one_more, base_ingredients):
            return self._binary_search_quantity(test_quantity + 1, upper_bound, product, base_ingredients)
        return test_quantity



def main():
    input_reader = RecipeReader()
    input_file = "Advent20191214_1_input.txt"
    recipes = input_reader.recipes(input_file)
    factory = NanoFactory(recipes)
    fuel_possible_to_produce = factory.quantity_possible_to_produce("FUEL", {"ORE": 1000000000000})
    print(fuel_possible_to_produce)



if __name__ == "__main__":
    main()