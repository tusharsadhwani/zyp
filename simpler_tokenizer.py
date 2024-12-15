import json
import sys
import tokenize

def string_offset_to_byte_offset(
    location: tuple[int, int],
    sourcelines: list[str],
) -> tuple[int, int]:
    line, column = location
    source_line = sourcelines[line-1]
    byte_offset = len(source_line[:column].encode())
    return (line, byte_offset)


def main() -> None:
    filepath = sys.argv[1]

    with open(filepath, "r", newline="") as file:
        source = file.read()
    # Ensure all line endings have newline as a valid index
    if len(source) == 0 or source[-1] != "\n":
        source = source + "\n"
    
    sourcelines = source.splitlines(keepends=True)
    # For that last newline token that exists on an imaginary line sometimes
    sourcelines.append("\n")

    with open(filepath, "rb") as file:
        iterator = tokenize.tokenize(file.readline)
        next(iterator)  # ignore encoding
        tokens = [
            {
                "type": tokenize.tok_name[token.type],
                "start": string_offset_to_byte_offset(token.start, sourcelines),
                "end": string_offset_to_byte_offset(token.end, sourcelines),
            }
            for token in iterator
        ]

    json.dump(tokens, sys.stdout)


if __name__ == "__main__":
    main()
