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
    check_sums = [tree.total_descendent_count() for tree in forest]
    print(check_sums)

def _read_input(filename):
    with open(filename, "r") as file:
        contents = [line.rstrip() for line in file]
    return [x.split(')') for x in contents]

if __name__ == "__main__":
    main()