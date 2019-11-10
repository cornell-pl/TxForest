
type filepath = string [@@deriving show]
type var = string [@@deriving show]
type antiQuotation = string [@@deriving show]
type forest_regexp_str = string [@@deriving show]


(** These are file paths, variable names, quoted OCaml code, and types used to
    generate regular expressions respectively *)

type 'a loc = 'a Location.loc [@@deriving show]

type forest_regex =
| Glob of forest_regexp_str loc
| Regex of forest_regexp_str loc [@@deriving show]

type generator =
| Matches of forest_regex loc
| InSet of antiQuotation loc [@@deriving show]

type qualifier =
| Guard of antiQuotation loc
| Generator of var * generator loc  [@@deriving show]

type surfaceSpec = 
| Var of var
| Pads of var
| File 
| Link 
| Option of surfaceSpec loc 
| Directory of (var loc * surfaceSpec loc) list 
| Comprehension of surfaceSpec loc * (qualifier loc) list
| PathExp of antiQuotation loc * surfaceSpec loc
| Predicate of surfaceSpec loc * antiQuotation loc [@@deriving show]
