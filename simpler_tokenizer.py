import json
import sys
import tokenize

def string_offset_to_byte_offset(
    location: tuple[int, int],
    sourcelines: list[bytes],
    encoding: str
) -> tuple[int, int]:
    line, column = location
    source_line = sourcelines[line-1]
    byte_offset = len(source_line.decode(encoding)[:column].encode(encoding))
    return (line, byte_offset)


def main() -> None:
    filepath = sys.argv[1]

    with open(filepath, "rb") as file:
        try:
            encoding, read_bytes = tokenize.detect_encoding(file.readline)
        except SyntaxError:
            # Broken encoding, bail on this file
            return json.dump([], sys.stdout)

        source = b"".join(read_bytes) + file.read()
    # Ensure all line endings have newline as a valid index
    if len(source) == 0 or source[-1:] != b"\n":
        source = source + b"\n"

    # Same as .splitlines(keepends=True), but doesn't split on linefeeds i.e. \x0c
    sourcelines = [line + b"\n" for line in source.split(b"\n")]
    # For that last newline token that exists on an imaginary line sometimes
    sourcelines.append(b"\n")

    with open(filepath, "rb") as file:
        iterator = tokenize.tokenize(file.readline)
        next(iterator)  # ignore encoding
        tokens = [
            {
                "type": tokenize.tok_name[token.type],
                "start": string_offset_to_byte_offset(token.start, sourcelines, encoding),
                "end": string_offset_to_byte_offset(token.end, sourcelines, encoding),
            }
            for token in iterator
        ]

    json.dump(tokens, sys.stdout)


if __name__ == "__main__":
    main()
