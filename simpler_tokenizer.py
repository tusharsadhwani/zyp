import json
import sys
import tokenize


def main() -> None:
    filepath = sys.argv[1]

    with open(filepath, "rb") as file:
        iterator = tokenize.tokenize(file.readline)
        next(iterator)  # ignore encoding
        tokens = [
            {
                "type": tokenize.tok_name[token.type],
                "start": token.start,
                "end": token.end,
            }
            for token in iterator
        ]

    json.dump(tokens, sys.stdout)


if __name__ == "__main__":
    main()
