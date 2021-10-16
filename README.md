# Naml

## Build

* requirements
  * opam

```sh
opam install ppx_deriving dune
opam exec -- dune build         # build
opam exec -- dune exec naml ... # run compiler
opam exec -- dune runtest       # test
opam exec -- dune build @fmt    # format
```
