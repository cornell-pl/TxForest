# TxForest
TxForest is a DSL (Domain Specific language) for FileSystems.

[paper](http://www.cs.cornell.edu/~dilorenzo/docs/txforest.pdf)

## requirements
- ocaml (version 4.05)
- dune
- core
- async
- menhir
- ppx_deriving
- ppx_deriving_yojson

## compling
``` make ```

## running examples
``` ./executables/<ex to run>.exe ```


## debugging

### ppx
to see the ppx rewriten version of an example:
```ocamlc -dsource _build/default/examples/<example to look at>.pp.ml```

