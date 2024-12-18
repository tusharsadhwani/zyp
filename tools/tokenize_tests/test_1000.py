# Some regressions that I found while testing
{}


# Comments must come before dedent, even if it looks like this:
def foo() -> None:
    ...

# <-- this comment comes before the dedent token

a = 10
a -= 1


def foo():
    x = \
        5  # <- ensure this doesn't add an indent
    print(x)
