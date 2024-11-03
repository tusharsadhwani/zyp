# can, in theory, rewrite this file in zig and using our own zig parser instead.
import ast
import os


class CheckTokenizeSourceExtractor(ast.NodeVisitor):
    def __init__(self) -> None:
        super().__init__()
        self.sources: set[str] = set()

    def visit_Call(self, node: ast.Call) -> None:
        super().generic_visit(node)

        func_name_node = node.func
        if not isinstance(func_name_node, ast.Attribute):
            return

        if func_name_node.attr == "check_tokenize" and len(node.args) == 2:
            first_arg = node.args[0]
            if isinstance(first_arg, ast.Constant) and isinstance(first_arg.value, str):
                self.sources.add(first_arg.value)


def main() -> None:
    test_folder_filepath = os.path.join(os.path.dirname(__file__), "tokenize_tests")
    test_tokenize_filepath = os.path.join(test_folder_filepath, "test_tokenize.py")
    with open(test_tokenize_filepath, "rb") as file:
        tree = ast.parse(file.read())

    visitor = CheckTokenizeSourceExtractor()
    visitor.visit(tree)
    for index, source in enumerate(sorted(visitor.sources), start=1):
        filepath = os.path.join(test_folder_filepath, f"test_{index:>04}.py")
        with open(filepath, "w") as file:
            file.write(source)


if __name__ == "__main__":
    main()
