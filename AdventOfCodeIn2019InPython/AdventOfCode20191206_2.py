class Tree:
    def __init__(self, value, child_trees = None):
        self.value = value
        self.children = [] if child_trees is None else child_trees

    def add_child(self, tree):
        self.children.append(tree)

    def child_count(self):
        return len(self.children)

    def decendent_count(self):
        return self.child_count() + sum([child.decendent_count() for child in self.children])

    def total_descendent_count(self):
        return self.decendent_count() + sum([child.total_descendent_count() for child in self.children])

    def path_to_node(self, node_value):
        if self.value == node_value:
            return [self.value]
        for child in self.children:
            child_path = child.path_to_node(node_value)
            if not child_path is None:
                return [self.value] + child_path
        return None

    def path_between_nodes(self, node1_value, node2_value):
        if self.value == node1_value:
            return self.path_to_node(node2_value)
        if self.value == node2_value:
            return [vertex for vertex in reversed(self.path_to_node(node1_value))]
        root_path1 = self.path_to_node(node1_value)
        root_path2 = self.path_to_node(node2_value)
        if root_path1 is None or root_path2 is None:
            return None
        first_differing_index = self._first_differing_index(root_path1, root_path2)
        return [vertex for vertex in reversed(root_path1[first_differing_index:])] + [root_path1[first_differing_index - 1]] + root_path2[first_differing_index:]


    def _first_differing_index(self, list1, list2):
        shorter_length = min(len(list1), len(list2))
        for index in range(shorter_length):
            if list1[index] != list2[index]:
                return index
        return shorter_length


class ForestBuilder:
    def build_forest(self, edges):
        nodes = self._tree_nodes(edges)
        self._add_edges_to_nodes(nodes, edges)
        non_root_nodes = set([child_tree.value for tree in nodes.values() for child_tree in tree.children])
        return [nodes[vertex] for vertex in nodes.keys() if not vertex in non_root_nodes]

    def _tree_nodes(self, edges):
        distinct_verteces = set(self._flatten_tuple_list(edges))
        return {vertex_value: Tree(vertex_value) for vertex_value in distinct_verteces}

    def _flatten_tuple_list(self, tuples):
        return [item for tpl in tuples for item in tpl]

    def _add_edges_to_nodes(self, nodes, edges):
        for (parent, child) in edges:
            nodes[parent].add_child(nodes[child])

def main():
    forest_builder = ForestBuilder()
    edges = _read_input("Advent20191206_1_input.txt")
    forest = forest_builder.build_forest(edges)
    first_tree = forest[0]
    path = first_tree.path_between_nodes('YOU', 'SAN')
    number_of_orbit_transfers = len(path) - 3
    print(number_of_orbit_transfers)

def _read_input(filename):
    with open(filename, "r") as file:
        contents = [line.rstrip() for line in file]
    return [x.split(')') for x in contents]

if __name__ == "__main__":
    main()