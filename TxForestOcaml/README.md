# TxForest (Ocaml)

## requirements
- ocaml (version 4.05)
- dune
- core
- async
- menhir
- ppx_deriving
- ppx_deriving_yojson

## code structure:

    TxForest
      examples
                various examples of using the the TxForest
                Library and embeded language
      parsing
                parser, lexer, helper functions and the low
                level forsts types for the languge
      ppx
                ppx rewriter to embed the language in ocaml
      src
                the forest library. eval forest does the
                computation, forestIntf is a nice wrapper
                over this to make it easier to work with


## compling
``` make ```

## running examples
``` ./executables/<ex to run>.exe ```

## debugging

### ppx
to see the ppx rewriten version of an example:


```ocamlc -dsource _build/default/examples/<example to look at>.pp.ml```