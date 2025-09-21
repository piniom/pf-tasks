
# Parser Project

Evaluator for SKI combinator and lambda calculuses. It showcases normal vs applicative strategies.

## Modules

- `ski.ml` - evaluator for ski 
- `lambda.ml` - evaluator for lambda terms

## Building

To build the project, ensure you have [OCaml](https://ocaml.org/) and [Dune](https://dune.build/) installed. Then run:

```sh
dune build
```

## Running

To run the ski evaluator (after building):

```sh
dune exec src/ski.exe
```

To run the lambda evaluator (after building):

```sh
dune exec src/lambda.exe
```
