# zyp

Ultra fast python parser, written in Zig.

> Currently WIP, the tokenizer is already production-grade, parser is under work.

## Local development / Testing

You'll need a Zig compiler, v0.13 or newer.

- To run basic tests, do

  ```bash
  zig build test
  ```

- To run the Python test suite, do:

  ```bash
  zig build test_suite
  ```

  You can also run the test suite on a specific Python file, or an entire folder
  (like your `venv`), and it'll validate against each Python file inside it.

  ```bash
  zig build test_suite -- ./venv
  ```


  This will help catch any bugs and regressions.
