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

# Thanks to black's test cases for these ones:
x = 1E+1.imag
x = 1E-1.real
x = 1+2.3e+7j
x = 1.e4 + 7.j
1e

# Ensure that this one DOES add an indent
class C:
\
    x = 1

# And no indent in between here
class C:
  \
    """Some
    \
    Docstring
    """

# this is another weird case
def bob(): \
         # pylint: disable=W9016
    pass

# form feed before 'def' here, don't indent
def bar():
    pass

x = 1

def bob():
    pass

x  # <- but we SHOULD output a dedent here

# Now, ensure we don't put a dedent on the form feed line, but on the line below.
def foo():
    pass

x = (
 \
2
)

# numbers with underscore
1_000_000

# Barry as FLUFL
1 <> 2

# And the file should be able to end in a comment.