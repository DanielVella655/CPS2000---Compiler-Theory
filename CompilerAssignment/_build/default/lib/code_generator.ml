open Parser
open Type_checker

(* helper functions that groups variables of same type, used for fields and locals *)
let group_by_type vars =
  let rec add_to_group acc (name, typ) =
    match acc with
    | [] -> [(typ, [name])]
    | (curr_typ, names) :: rest -> (
      if curr_typ = typ then
        (curr_typ, names @ [name]) :: rest
      else
        (curr_typ, names) :: add_to_group rest (name, typ) )
  in
  List.fold_left add_to_group [] vars

let gen_path path =
  (* convert ResIR path ["str1", "str2"] into C struct str1_str2 *)
  String.concat "_" path
  
let gen_local local =
  (* strip leading % *)
  let (Parser.Local name) = local in
    String.sub name 1 (String.length name - 1)

let gen_label label =
  (* keeping labels as they are named in ResIR *)
  let (Label name) = label in
    name

let rec gen_type resir_type =
  (* match each ResIR type to its corresponding C-type *)
  match resir_type with
  | Prim Bool -> "bool"
  | Prim I32  -> "int32_t"
  | Prim I64  -> "int64_t"
  | Prim U32  -> "uint32_t"
  | Prim F64  -> "double"
  | Path p    -> gen_path p
  | Ptr t     -> gen_type t ^ "*"

let gen_literal literal =
  (* map literals to C99 literals (only NULL is different) *)
  match literal with
  | NullLit -> "NULL"
  | _       -> string_of_literal literal

let gen_unop unop =
  (* matching to C99 operators *)
  match unop with
  | Neg -> "-"
  | Not -> "!"

let gen_binop binop =
  (* matching to C99 operators *)
  match binop with
  | Add -> "+"  | Sub -> "-"  | Mul -> "*" | Div -> "/"  | Mod -> "%" 

  | Eq  -> "==" | Ne  -> "!=" | Lt  -> "<" | Le  -> "<=" | Gt  -> ">"  | Ge  -> ">="

  | And -> "&&" | Or  -> "||"

let gen_rhs rhs =
  (* match RHS to C99 equivalent *)
  match rhs with
  | RhsLocal loc            -> gen_local loc
  | RhsConst lit            -> gen_literal lit
  | RhsCast (loc, typ)      -> Printf.sprintf "(%s)%s" (gen_type typ) (gen_local loc)
  (* for example (int32_t(x))*)

  | RhsUn (op, loc)         -> Printf.sprintf "%s%s" (gen_unop op) (gen_local loc)
  (* for example (-x)*)

  | RhsBin (op, loc1, loc2) -> Printf.sprintf "%s %s %s" (gen_local loc1) (gen_binop op) (gen_local loc2)
  (* for example (x + y) *)

  | RhsAddrOf loc           -> Printf.sprintf "&%s" (gen_local loc)
  (* for example (&x) *)

  | RhsMemberPtr (loc, str) -> Printf.sprintf "&(%s->%s)" (gen_local loc) str
  (* for example (&(x->Custom))*)

  | RhsLoad loc             -> Printf.sprintf "*%s" (gen_local loc)
  (* for example *x *)

  | RhsStore (loc1, loc2)   -> Printf.sprintf "*%s = %s" (gen_local loc1) (gen_local loc2)
  (* for example *x = y *)

  | RhsCall (path, locs)    -> Printf.sprintf "%s(%s)" (gen_path path) (String.concat ", " (List.map gen_local locs))
  (* for example Math_abs_diff(x, y) *)

let gen_stmt stmt =
  (* handle RHS separately because it self-contains = *)
  match stmt.rhs with
  | RhsStore _ -> "    " ^ gen_rhs stmt.rhs ^ ";"
  | _ ->
    match stmt.dest with
    | Some loc -> Printf.sprintf "    %s = %s;" (gen_local loc) (gen_rhs stmt.rhs)
    | None     -> Printf.sprintf "    %s;" (gen_rhs stmt.rhs)

let gen_term term =
  (* jump or return accordingly *)
  match term with
  | Jump lab                -> Printf.sprintf "    goto %s;" (gen_label lab)
  | Cjump (loc, lab1, lab2) -> Printf.sprintf "    if (%s) goto %s; else goto %s;" (gen_local loc) (gen_label lab1) (gen_label lab2)
  | Return None             -> "    return;"
  | Return (Some loc)       -> Printf.sprintf "    return %s;" (gen_local loc)

let gen_block block =
  (* build label blocks *)
  let stmts_str = String.concat "\n" (List.map gen_stmt block.stmts) in
    Printf.sprintf "%s:\n%s\n%s" (gen_label block.label) (stmts_str) (gen_term block.term)

let gen_param param =
  (* join local to its type *)
  let (loc, typ) = param in
    Printf.sprintf "%s %s" (gen_type typ) (gen_local loc)

let gen_ret_type ret_type =
  (* type including void *)
  match ret_type with
  | Void -> "void"
  | RetType typ -> gen_type typ

let gen_extern extern =
  (* group vars then build it as a struct *)
  let grouped_line (typ, names) =
    let names_str = String.concat ", " names in 
      Printf.sprintf "    %s %s;" (gen_type typ) (names_str)
  in
  let fields_str = String.concat "\n" (List.map grouped_line (group_by_type extern.fields)) in
    Printf.sprintf "typedef struct %s {\n%s\n} %s;" (gen_path extern.path) (fields_str) (gen_path extern.path) 

let gen_funct (funct : funct) =
  (* group locals then build label blocks *)
  let grouped_line (typ, locals) =
    let names_str = String.concat ", " (List.map gen_local locals) in 
      Printf.sprintf "    %s %s;" (gen_type typ) (names_str)
  in
  let locals_str = String.concat "\n" (List.map grouped_line (group_by_type funct.locals)) in
  Printf.sprintf "%s %s(%s) {\n%s\n    goto %s;\n\n%s\n}" (gen_ret_type funct.ret_type) (gen_path funct.path)
    (String.concat ", " (List.map gen_param funct.params)) (locals_str) (gen_label funct.entry)
    (String.concat "\n\n" (List.map gen_block funct.blocks))

let gen_program program =
  (* add headers for ints, bool, and NULL *)
  let (stripped_program, _) = program in
  let externs_str = (
    if stripped_program.externs <> [] then 
      "\n\n" ^ String.concat "\n\n" (List.map gen_extern stripped_program.externs)
    else
      "" ) in
    Printf.sprintf "#include <stdint.h>\n#include <stdbool.h>\n#include <stddef.h>%s\n\n%s" 
    (externs_str) (gen_funct stripped_program.funct)