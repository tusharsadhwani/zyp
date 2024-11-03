# zyp

Ultra fast python parser, written in Zig.

> Currently a work in progress, the tokenizer is close to ready, parser is next.

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

  This will catch any major regressions.
