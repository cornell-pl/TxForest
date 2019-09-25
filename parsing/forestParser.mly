%{ open ForestTypes
   open Location
   let make_loc p1 p2 =
     { loc_start = p1;
       loc_end = p2;
       loc_ghost = false;
     }

   let loc_of p1 p2 n = Ppxlib.Loc.make (make_loc p1 p2) n
   let wrap_str str = {str with txt = "\"" ^ str.txt ^ "\""}
%}

(* Forest *)
%token <string> ID
%token <string> STRING
%token <string> AQUOT
%token LPAREN
%token RPAREN
%token EOF


%token FILE
%token DIR
%token LINK
%token OPT
%token PADS
%token WHERE

%token DCOLON

%token SEMICOLON
%token EQ
%token LBRACE
%token RBRACE
%token IS

%token LBRACK
%token RBRACK
%token COMMA
%token BAR
%token MATCHES
%token RE
%token GL
%token BARROW



(* Forest *)


%type <ForestTypes.surfaceSpec ForestTypes.loc> f_ast f_cons f_path_ast f_bast f_cons_ast f_last
%type <ForestTypes.antiQuotation ForestTypes.loc> path_exp
%type <ForestTypes.qualifier ForestTypes.loc> qualifier
%type <ForestTypes.forest_regex ForestTypes.loc> match_statement
%type <ForestTypes.generator ForestTypes.loc> generator_statement
%type <ForestTypes.var ForestTypes.loc * ForestTypes.surfaceSpec ForestTypes.loc> direntry


(* Starts *)

%start <(ForestTypes.var ForestTypes.loc * ForestTypes.surfaceSpec ForestTypes.loc) list> forest_prog
%%


(* Forest parsing *)
forest_prog: l = nonempty_list(separated_pair(id,EQ,f_ast)); EOF  { l } ;

f_ast:
  | f = f_path_ast; WHERE; s = aquot {Predicate (f,s) |> loc_of $startpos(f) $endpos(s) }
  | f_path_ast { $1 }
  ;



f_path_ast:
  | p = path_exp; DCOLON ; f=f_path_ast { PathExp (p,f) |> loc_of $startpos(p) $endpos(f) }
  | f_bast { $1 }
  ;

f_bast:
  | f = f_bast; OPT { Option f |> loc_of $startpos(f) $endpos($2) }
  | f_cons_ast { $1 }
  ;

f_cons_ast:
  | f_cons { $1 }
  | f_last { $1 }
  ;

f_last:
  | DIR; LBRACE; list = separated_or_terminated_nonempty_list(SEMICOLON,direntry) ; RBRACE 
     { Directory (list) |> loc_of $startpos($1) $endpos($4) }
  | LBRACK; f = f_ast; BAR ; pl = separated_nonempty_list(COMMA,qualifier); RBRACK
    { Comprehension (f,pl) |> loc_of $symbolstartpos $endpos($5)}
      (*TODO: Make sure you want symbolstartpos here *)
  | x = ID { Var (x) |> loc_of $startpos(x) $endpos(x) }
  | LPAREN; f = f_ast; RPAREN { f }
  ;

f_cons:
  | FILE { loc_of $startpos $endpos File}
  | LINK { loc_of $startpos $endpos Link}
  | PADS; x = ID  { Pads(x) |> loc_of $startpos($1) $endpos(x) }
  ;

path_exp:
  | string { wrap_str $1 }
  | ID { loc_of $startpos $endpos $1}
  | aquot { $1 }
  ;

qualifier:
  | x = ID ; BARROW ; g = generator_statement {Generator(x,g) |> loc_of $startpos(x) $endpos(g)}
  | aquot {Guard($1) |> loc_of $startpos $endpos}
  ;

generator_statement:
  | MATCHES; regexp = match_statement { Matches(regexp) |> loc_of $startpos($1) $endpos(regexp) }
  | aquot { InSet($1) |> loc_of $startpos $endpos }
  ;


match_statement:
  | RE; regexp = string { Regex (regexp) |> loc_of $startpos($1) $endpos(regexp) }
  | GL; regexp = string { Glob (regexp) |> loc_of $startpos($1) $endpos(regexp) }
  ;


id:
  | ID { loc_of $startpos $endpos $1}

string:
  | STRING { loc_of $startpos $endpos $1}

aquot:
  | AQUOT { loc_of $startpos $endpos $1}

direntry: separated_pair(id,IS,f_ast) { $1 }

(* Directly stolen from OCaml Parser *)
(* [separated_or_terminated_nonempty_list(delimiter, X)] recognizes a nonempty
   list of [X]s, separated with [delimiter]s, and optionally terminated with a
   final [delimiter]. Its definition is right-recursive. *)

separated_or_terminated_nonempty_list(delimiter, X):
  | x = X ioption(delimiter)
      { [x] }
  | x = X
    delimiter
    xs = separated_or_terminated_nonempty_list(delimiter, X)
      { x :: xs }
