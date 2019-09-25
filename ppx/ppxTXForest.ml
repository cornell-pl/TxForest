open Parser_helper
open ForestTypes
open Ppxlib
open Core
open Ast_builder.Default

type varType =
  | ZipVar
  | StrVar

let spec_name = Printf.sprintf "%s_spec"
let spec_loc loc = {loc with txt = spec_name loc.txt}

let mkFun ~loc ocaml = [%expr fun fs (dirEnv, compEnv) -> [%e ocaml]][@metaloc ocaml.loc]

let parse_ocaml ~loc str : Parsetree.expression =
  let open Lexing in
  let open Location in
  let lexbuf = Lexing.from_string str in
  let lexbuf = {lexbuf with
    lex_start_p = loc.loc_start;
    lex_curr_p = loc.loc_start;
  } in
  Parse.expression lexbuf

(* TODO: Use the context to keep variables that have been remapped (with lets e.g.) *)
let manipulate_vars varmap = object
  inherit [String.Set.t] Ast_traverse.map_with_context as super

  method! expression ctxt e =
    let processed_e = super#expression ctxt e in
    match processed_e.pexp_desc with
    | Pexp_ident {loc;txt = Lident s} ->
      begin match Map.find varmap s with
      | None -> processed_e
      | Some ZipVar -> 
        [%expr let (p,z) = ForestIntf.get_var dirEnv [%e estring ~loc s] in (fs,p,empty_ps,z)]
      | Some StrVar -> [%expr ForestIntf.get_var compEnv [%e estring ~loc s]]
      end
    | _ -> processed_e
end

let process_ocaml varmap {loc; txt} = 
  parse_ocaml ~loc txt 
  |> (manipulate_vars varmap)#expression String.Set.empty

let extract_var = function
  | `Yes v 
  | `No v -> v
  | `Unknown -> failwith "There must be at least one generator in a comprehension"


let process_qualifiers ~loc varmap quals =
  let process_generator (var : string loc) {loc; txt} =
    match txt with
    | InSet ocaml -> (`No var), process_ocaml varmap ocaml
    | Matches {txt; loc} ->
      let regMatch = 
        match txt with
        | Glob {loc; txt} ->
          [%expr ForestIntf.glob_match_from_string [%e estring ~loc txt]]
        | Regex {loc; txt} -> 
          [%expr ForestIntf.regexp_match_from_string [%e estring ~loc txt]]
      in
      let expr = [%expr fetch_dir dir' |> String.Set.filter ~f:([%e regMatch])] 
        |> (manipulate_vars (String.Map.singleton "dir'" ZipVar))#expression String.Set.empty 
      in
      (`Yes var), expr
  in 
  let process_qualifier (dir,list) {loc;txt} =
    match dir,txt with
    | `Unknown, Guard _ ->  failwith "Guards must come after generators in comprehensions"
    | v, Guard ocaml -> 
      let v = extract_var v in
      let guardExp = 
        [%expr String.Set.filter ~f:(fun [%p ppat_var ~loc:(v.loc) v] -> 
            [%e process_ocaml varmap ocaml]) set
        ]
      in
      dir, (guardExp :: list)
    | `Unknown, Generator (var, gen) -> 
        let dir,genExp = process_generator (Loc.make ~loc var) gen in
        dir, (genExp :: list)
    | v, Generator (v2,_) -> 
      failwithf "Comprehensions do not currently support multiple generators. Given generators for %s and %s" (extract_var v).txt v2 ()
  in
  let (dir,expL) = List.fold ~init:(`Unknown, []) ~f:process_qualifier quals in
  let expr = List.fold expL ~init:(evar ~loc "set") 
    ~f:(fun acc exp -> [%expr let set = [%e exp] in [%e acc]])
  in
  (dir,expr)

let rec generate_def varmap {txt; loc} : Parsetree.expression =
  begin
    match txt with
    | Var x -> spec_name x |> evar ~loc
    | File -> [%expr File]
    | Option ast -> [%expr Opt  [%e generate_def varmap ast]]
    | Directory asts ->
        let (_,list) = 
          List.fold asts ~init:(varmap,[]) ~f:(fun (map,acc) (var,ast) ->
            let map = Map.set ~key:var.txt ~data:ZipVar map in
            (map, (var, generate_def map ast) :: acc)
          )
        in
        List.fold list ~init:([%expr Null]) ~f:(fun exp ({loc;txt},def) ->
          [%expr DPair [%e pexp_tuple ~loc [estring ~loc txt; def; exp]]]
        )
    | Comprehension (ast, qualifiers) ->
        let (dir, qualExp) = process_qualifiers ~loc varmap qualifiers in
        let {loc = vloc;txt} = extract_var dir in
        let def = generate_def (Map.set ~key:txt ~data:StrVar varmap) ast in
        let compExp = 
          [%expr Comp [%e pexp_tuple ~loc [def; estring ~loc:vloc txt; mkFun ~loc qualExp]]]
        in 
        begin
          match dir with
          | `Yes _ -> [%expr DPair ("dir'",Dir, [%e compExp])]
          | `No _ -> compExp
          | `Unknown -> failwith "There must be at least one generator in a comprehension"
        end
    | PathExp (ocaml,ast) -> 
        let ocamlExp = process_ocaml varmap ocaml in
        let def = generate_def varmap ast in
        [%expr PathExp [%e pexp_tuple ~loc [mkFun ~loc ocamlExp;def]]]
    | Predicate (ast,ocaml) -> 
        let ocamlExp = process_ocaml (Map.set ~key:"this" ~data:ZipVar varmap) ocaml in
        let def = generate_def varmap ast in
        [%expr DPair ("this", [%e def],Pred [%e mkFun ~loc ocamlExp])]

    | Link -> failwith "Links are not currently supported"
    | Pads _ -> failwith "Pads is not currently supported"
  end 

let generate_binding (name,ast) = 
  let expr = generate_def String.Map.empty ast in
  let pat = spec_loc name |> ppat_var ~loc:name.loc in
  value_binding ~loc:ast.loc ~pat ~expr

let convert_to_decls ~loc ~path fstr =
  let structs = ParserHelper.forest_parse_string ~loc fstr |> List.map ~f:generate_binding in
  pstr_value ~loc Recursive structs

(* Debugging code:
let d s = Core.Printf.printf "Debug: %s\n%!" s

let show_surfaceAst s = show_ast pp_surfaceSpec s

(* Parsed structures *)
  ParserHelper.forest_parse_string loc fstr
  |> Core.List.iter ~f:(fun (_,s) -> d (show_surfaceAst s));

(* Post AST making: *)
  Pprintast.expression Format.std_formatter expr;
*)

let txforest_extension = 
  let open Ast_pattern in
  let string_expr =
    (* Matches {| code |} *)
    string ""
    |> some
    |> pconst_string __
    |> pexp_constant
  in
  let structure_match internals = pstr (pstr_eval internals nil ^:: nil) in
  let ext_keyword = "txforest" in
  Extension.declare 
    ext_keyword
    Extension.Context.structure_item
    (structure_match string_expr)
    convert_to_decls
  ;;

Driver.register_transformation "txforest" ~extensions:[txforest_extension]