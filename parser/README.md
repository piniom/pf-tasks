# Parser Project

This project is an parser for something that resembles a functional language.

## Structure

- `src/` - Source code directory
  - `ast.ml` - Abstract Syntax Tree definitions
  - `lexer.ml` - Lexer implementation
  - `parser.ml` - Parser implementation
  - `token.ml` - Token definitions
  - `main.ml` - Entry point for the parser
  - `dune` - Dune build configuration for the source
- `dune-project` - Dune project file
- `_build/` - Build output directory (auto-generated)

## Building

To build the project, ensure you have [OCaml](https://ocaml.org/) and [Dune](https://dune.build/) installed. Then run:

```sh
dune build
```

## Running

To run the parser (after building):

```sh
dune exec src/main.exe
```

## Running Tests

To run the tests:

```sh
dune exec src/ast_test.exe
```

