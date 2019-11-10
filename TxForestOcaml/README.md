# TxForest (Ocaml)
This includes:
  - forest library implmenting the logic of a local "thread" as discussed in the paper
  - ppx rewriter to fully embed the surface syntax for the language in ocmal
  - a server to enforce the global logic discussed in the paper
  - a client to run a universal filesystem specification and allow runnning through commands
  - many examples running on top of the local version of forest (soon to be ported to the global version)
  - multiple filesystem libraries for our library to run on top of (in memory FS, posix FS, write back cache style posix FS using in memory cache, and soon to contain write back cache style posix FS using disk cache)

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
      lib
                the exposed forest libary (ForestIntf) containing
                local and global transactional versions of the
                library. Also includes a client to walk through the
                filesystem using forest commands running on a
                universal filesystem.
      src
                implementation of the core calculus of forest.
                Txforest runs forest commands as local transactions
                like the denotation function in the paper. TxForestGlobal
                implements the global loging and optimistic checking for
                the global semantics specified in the paper.
                This also includes the filesystem libraries which forest
                can run on top of (SimpleFilesystem is in memory,
                PosixFilesystem
                runs on a disk filestem, UncommitedPosixFilesystem reads
                from a disk filesystem and writes to an in memory one,
                ZFSFilesystem runs on top of ZFS, soon this will also
                include another library which reads and writes
                from a temp disk filesystem).


## compling
``` make ```

## running examples
``` ./executables/<ex to run>.exe ```

## debugging

### ppx
to see the ppx rewriten version of an example:


```ocamlc -dsource _build/default/examples/<example to look at>.pp.ml```