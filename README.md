# TxForest
TxForest is a DSL (Domain Specific language) for FileSystems.

paper writen about this DSL: [paper](http://www.cs.cornell.edu/~dilorenzo/docs/txforest.pdf)

related and original repo of this work (contains ZFS a related DSL and the source latex of the paper on this project): [ZFS repo](https://github.com/cornell-pl/ocaml-zfs)

This repo contains 2 implementations
- an ocaml version:
  - forest library implmenting the logic of a local "thread" as discussed in the paper
  - ppx rewriter to fully embed the language in ocmal
  - a server to enforce the global logic discussed in the paper
  - a client to run a universal filesystem specification and allow runnning through commands
  - many examples running on top of the local version of forest (soon to be ported to the global version)
  - multiple filesystem libraries for our library to run on top of (in memory FS, posix FS, write back cache style posix FS using in memory cache, and soon to contain write back cache style posix FS using disk cache)
- a python version:
  - the forest library in in progress



