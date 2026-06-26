open Lexer

(* Basic Components and Identifiers *)
type path = string list   (* ["Custom"; "Struct"; "Point"] *)
type local = Local of string       (* "%var" *)
type label = Label of string       (* "bb0" *)

type prim_type = | Bool | I32 | I64 | U32 | F64

type resir_type = | Prim of prim_type | Path of path | Ptr of resir_type (* ptr<Type> *)

type literal = | IntLit of int | FloatLit of float | BoolLit of bool | NullLit

(* Operators and Right-Hand Side Expressions *)
type unop = | Neg | Not

type binop = 
| Add | Sub | Mul | Div | Mod 
| Eq  | Ne  | Lt  | Le  | Gt 
| Ge  | And | Or

type rhs = 
| RhsLocal     of local
| RhsConst     of literal
| RhsCast      of local * resir_type
| RhsUn        of unop * local
| RhsBin       of binop * local * local
| RhsAddrOf    of local
| RhsMemberPtr of local * string
| RhsLoad      of local
| RhsStore     of local * local
| RhsCall      of path * local list        (* where local list represents Args *)

(* Statements, Terminators, and Blocks *)
type stmt = {
  dest : local option;
  rhs  : rhs;
}

type term = 
| Jump of label 
| Cjump of local * label * label
| Return of local option

type block = {
  label : label;
  stmts : stmt list;
  term  : term;
}

(* Program Structure *)
type param = local * (resir_type)

type ret_type = Void | RetType of resir_type

type field = string * resir_type

type extern_type = {
  path   : path;
  fields : field list;
}

type funct = {
  path     : path;
  params   : param list;
  ret_type : ret_type;
  locals   : (local * resir_type) list;
  entry    : label;
  blocks   : block list;
}

type program = {
  externs  : extern_type list;
  funct    : funct;
}

(* helper functions to print errors *)
let string_of_path path =
  String.concat "::" path

let string_of_local (Local name) =
  name

let string_of_label (Label name) =
  name

let string_of_prim_type typ =
  match typ with
  | Bool -> "bool" | I32 -> "i32" | I64 -> "i64" | U32 -> "u32" | F64 -> "f64"

let rec string_of_resir_type typ =
  match typ with
  | Prim p -> string_of_prim_type p | Path p -> string_of_path p | Ptr p -> "ptr<" ^ string_of_resir_type p ^ ">"

let string_of_literal lit =
  match lit with
  | IntLit i -> string_of_int i | FloatLit f -> string_of_float f | BoolLit b -> string_of_bool b | NullLit -> "null"

let string_of_unop op =
  match op with
  | Neg -> "neg" | Not -> "not"

let string_of_binop op =
  match op with
  | Add -> "add" | Sub -> "sub" | Mul -> "mul" | Div -> "div" | Mod -> "mod"
  | Eq  -> "eq"  | Ne  -> "ne"  | Lt  -> "lt"  | Le  -> "le"  | Gt  -> "gt"
  | Ge  -> "ge"  | And -> "and" | Or  -> "or"

let string_of_rhs rhs =
  match rhs with
  | RhsLocal loc            -> string_of_local loc
  | RhsConst cons           -> "const " ^ string_of_literal cons
  | RhsCast (loc,typ)       -> "cast " ^ string_of_local loc ^ " to " ^ string_of_resir_type typ
  | RhsUn (op, loc)         -> "un " ^ string_of_unop op ^ " " ^ string_of_local loc
  | RhsBin (op, loc1, loc2) -> "bin " ^ string_of_binop op ^ " " ^ string_of_local loc1 ^ ", " ^ string_of_local loc2
  | RhsAddrOf loc           -> "addr_of " ^ string_of_local loc
  | RhsMemberPtr (loc, str) -> "member_ptr " ^ string_of_local loc ^ ", " ^ str
  | RhsLoad loc             -> "load " ^ string_of_local loc
  | RhsStore (loc1, loc2)   -> "store " ^ string_of_local loc1 ^ ", " ^ string_of_local loc2
  | RhsCall (path, args)    ->
    let args_str = String.concat ", " (List.map string_of_local args) in
    "call " ^ string_of_path path ^ "(" ^ args_str ^ ")"

let string_of_stmt stmt =
  match stmt.dest with
  | Some loc -> string_of_local loc ^ " = " ^ string_of_rhs stmt.rhs
  | None     -> string_of_rhs stmt.rhs

let string_of_term term =
  match term with
  | Jump lbl                -> "jump " ^ string_of_label lbl
  | Cjump (loc, lbl1, lbl2) -> "cjump " ^ string_of_local loc ^ ", " ^ string_of_label lbl1 ^ ", " ^ string_of_label lbl2
  | Return (Some loc)       -> "return " ^ string_of_local loc
  | Return None             -> "return"

let string_of_block block =
  let stmts_str = List.map (fun str -> "    " ^ string_of_stmt str ^ ";") block.stmts in
  let body = String.concat "\n" (stmts_str @ ["    " ^ string_of_term block.term ^ ";"]) in
    "  " ^ string_of_label block.label ^ ":\n" ^ body

let string_of_extern ext =
  let fields_str = List.map (fun (name, typ) -> "  " ^ name ^ " : " ^ string_of_resir_type typ ^ ";") ext.fields in
    "extern type " ^ string_of_path ext.path  ^ " {\n" ^ String.concat "\n" fields_str ^ "\n}"

let string_of_function funct =
  let params_str = String.concat ", " (List.map (fun (loc, typ) -> string_of_local loc ^ ": " ^ string_of_resir_type typ) funct.params) in
  let ret_str = match funct.ret_type with Void -> "void" | RetType typ -> string_of_resir_type typ in
  let locals_block =
    if funct.locals = [] then "  locals {\n }"
    else
      let locals_str = List.map (fun (loc, typ) -> "    " ^ string_of_local loc ^ " : " ^ string_of_resir_type typ ^ ";") funct.locals in
      "  locals {\n" ^ String.concat "\n" locals_str ^ "\n  }" in
  let blocks_str = String.concat "\n\n" (List.map string_of_block funct.blocks) in

  "function " ^ string_of_path funct.path ^ "(" ^ params_str ^ ") -> " ^ ret_str ^ " {\n" ^ 
  locals_block ^ "\n" ^
  "  entry " ^ string_of_label funct.entry ^ ";\n\n" ^
  blocks_str ^ "\n}"

let print_parsed program =
  let externs_str = String.concat "\n\n" (List.map string_of_extern program.externs) in
    if program.externs = [] then string_of_function program.funct
    else externs_str ^ "\n\n" ^ string_of_function program.funct


let expect expected_token tokens =
  (* if expected_token is found it is discard, used for punctuation *)
  match tokens with
  | (t, _, _) :: rest when t = expected_token -> rest
  | (t, line, col) :: _ -> 
      let exp = string_of_tokens (expected_token, 0, 0) in
      let cleaned_exp = String.sub exp 1 (String.length exp - 8) in (* removes ( and trailing , 0, 0)*)
      let found = string_of_tokens (t, 0, 0) in
      let cleaned_found = String.sub found 1 (String.length found - 8) in
      failwith (Printf.sprintf "Syntax Error at Line %d, Col %d: Expected token \"%s\" but found token \"%s\"." line col cleaned_exp cleaned_found)
  | [] -> failwith "Syntax Error: Unexpected End of File."

let parse_ident tokens = 
  (* parsing identifier *)
  match tokens with
  | (Lexer.Ident name, _, _) :: rest -> (name, rest)
  | (t, line, col) :: _ ->
    failwith (Printf.sprintf "Syntax Error at Line %d, Col %d: Expected an identifier." line col)
  | [] -> failwith "Compiler Bug: Token stream empty without hitting EOF."

let parse_path tokens = 
  (* parsing path by parsing ident then looping over idents, skipping PathSep *)
  let (first_id, rest) = parse_ident tokens in
  let rec path_loop acc loop_tokens = 
    match loop_tokens with
    | (Lexer.PathSep, _, _) :: loop_tokens1 ->
      let (next_id, loop_tokens2) = parse_ident loop_tokens1 in
        path_loop (next_id :: acc) loop_tokens2
    | _ -> (List.rev acc, loop_tokens)

  in path_loop [first_id] rest

let parse_local tokens = 
  (* if local just grabs name *)
  match tokens with 
  | (Lexer.Local name, _, _) :: rest -> (Local name, rest)
  | (t, line, col) :: _ -> 
    failwith (Printf.sprintf "Syntax Error at Line %d, Col %d: Expected a local variable." line col)
  | [] -> failwith "Compiler Bug: Token stream empty without hitting EOF."

let parse_args tokens = 
  (* list of locals *)
  match tokens with 
  | (Lexer.RBracket, _, _) :: _ -> ([], tokens)
  | _ ->
    let (first_arg, rest1) = parse_local tokens in
      let rec args_loop acc loop_tokens =
        match loop_tokens with
        | (Lexer.Comma, _, _) :: loop_tokens1 ->
            let (new_arg, loop_tokens2) = parse_local loop_tokens1 in
              args_loop (new_arg :: acc) loop_tokens2
        | _ -> (List.rev acc, loop_tokens)
      in args_loop [first_arg] rest1

let parse_label tokens =
  (* if label just grabs name *)
  match tokens with
  | (Lexer.Label name, _, _) :: rest -> (Label name, rest)
  | (t, line, col) :: _ -> 
    failwith (Printf.sprintf "Syntax Error at Line %d, Col %d: Expected a label (for example bb0)." line col)
  | [] -> failwith "Compiler Bug: Token stream empty without hitting EOF."

let parse_prim tokens =
  (* parsing primitive types *)
  match tokens with 
  | (Lexer.Bool, _, _) :: rest -> (Bool, rest)
  | (Lexer.I32, _, _)  :: rest -> (I32,  rest)
  | (Lexer.I64, _, _)  :: rest -> (I64,  rest)
  | (Lexer.U32, _, _)  :: rest -> (U32,  rest) 
  | (Lexer.F64, _, _)  :: rest -> (F64,  rest)
  | (t, line, col) :: _ ->
    failwith (Printf.sprintf "Syntax Error at Line %d, Col %d: Expected a primitive type." line col)
  | [] -> failwith "Compiler Bug: Token stream empty without hitting EOF."

let rec parse_type tokens = 
  match tokens with
  (* wrapping primitive types in Prim *)
  | (Lexer.Bool, _, _) :: _
  | (Lexer.I32, _, _)  :: _
  | (Lexer.I64, _, _)  :: _
  | (Lexer.U32, _, _)  :: _ 
  | (Lexer.F64, _, _)  :: _ ->
    let (prim_node, rest) = parse_prim tokens in
    (Prim prim_node, rest)

  (* handling paths *)
  | (Lexer.Ident _, _, _) :: _ ->
    let (path_list, rest) = parse_path tokens in
    (Path path_list, rest)

  (* handling pointers *)
  | (Lexer.Ptr, _, _) :: rest ->
    let rest1 = expect Lexer.LAngle rest in
    let (inner_type, rest2) = parse_type rest1 in
    let rest3 = expect Lexer.RAngle rest2 in
    (Ptr inner_type, rest3)

  | (t, line, col) :: _ ->
    failwith (Printf.sprintf "Syntax Error at Line %d, Col %d: Expected a type specification." line col)

  | [] -> failwith "Compiler Bug: Token stream empty without hitting EOF."

let parse_literal tokens = 
  (* handling all diff types of literals *)
  match tokens with
  | (Lexer.IntLit num, _, _)   :: rest -> (IntLit num, rest)
  | (Lexer.FloatLit num, _, _) :: rest -> (FloatLit num,  rest)
  | (Lexer.True, _, _)         :: rest -> (BoolLit true,  rest)
  | (Lexer.False, _, _)        :: rest -> (BoolLit false,  rest) 
  | (Lexer.Null, _, _)         :: rest -> (NullLit,  rest)

  | (t, line, col) :: _ ->
    failwith (Printf.sprintf "Syntax Error at Line %d, Col %d: Expected a literal." line col)
  | [] -> failwith "Compiler Bug: Token stream empty without hitting EOF."

let parse_unop tokens =
  (* handling unary operators *)
  let rest1 = expect Lexer.Un tokens in
    match rest1 with
    | (Lexer.Ident "neg", _, _) :: rest2 -> (Neg, rest2)
    | (Lexer.Ident "not", _, _) :: rest2 -> (Not, rest2)

    | (Lexer.Ident bad_op, line, col) :: _ ->
      failwith (Printf.sprintf "Syntax Error at Line %d, Col %d: Expected a unary operator (\"neg\" or \"not\") but found \"%s\"." line col bad_op)

    | (t, line, col) :: _ -> 
      failwith (Printf.sprintf "Syntax Error at Line %d, Col %d: Expected a unary operator (\"neg\" or \"not\")." line col)
    | [] -> failwith "Compiler Bug: Token stream empty without hitting EOF."

let parse_binop tokens =
  (* handling binary operators *)
  let rest1 = expect Lexer.Bin tokens in
    match rest1 with
    | (Lexer.Ident "add", _, _) :: rest2 -> (Add, rest2)
    | (Lexer.Ident "sub", _, _) :: rest2 -> (Sub, rest2)
    | (Lexer.Ident "mul", _, _) :: rest2 -> (Mul, rest2)
    | (Lexer.Ident "div", _, _) :: rest2 -> (Div, rest2)
    | (Lexer.Ident "mod", _, _) :: rest2 -> (Mod, rest2)
    | (Lexer.Ident "eq", _, _)  :: rest2 -> (Eq, rest2)
    | (Lexer.Ident "ne", _, _)  :: rest2 -> (Ne, rest2)
    | (Lexer.Ident "lt", _, _)  :: rest2 -> (Lt, rest2)
    | (Lexer.Ident "le", _, _)  :: rest2 -> (Le, rest2)
    | (Lexer.Ident "gt", _, _)  :: rest2 -> (Gt, rest2)
    | (Lexer.Ident "ge", _, _)  :: rest2 -> (Ge, rest2)
    | (Lexer.Ident "and", _, _) :: rest2 -> (And, rest2)
    | (Lexer.Ident "or", _, _)  :: rest2 -> (Or, rest2)

    | (Lexer.Ident bad_op, line, col) :: _ ->
      failwith (Printf.sprintf "Syntax Error at Line %d, Col %d: Expected a binary operator (\"add\" or \"sub\" for example) but found \"%s\"." line col bad_op)
      
    | (t, line, col) :: _ -> 
      failwith (Printf.sprintf "Syntax Error at Line %d, Col %d: Expected a binary operator (\"add\" or \"sub\" for example)." line col)
    | [] -> failwith "Compiler Bug: Token stream empty without hitting EOF."

let parse_rhs tokens =
  (* handling all possible rhs *) 
  match tokens with 
  | (Lexer.Local _, _, _) :: _ -> 
    let (local_name, rest1) = parse_local tokens in
      (RhsLocal local_name, rest1)

  | (Lexer.Const, _, _) :: rest1  ->
    let (lit, rest2) = parse_literal rest1 in
      (RhsConst lit, rest2)

  | (Lexer.Cast, _, _) :: rest1 -> 
    let (local_name, rest2) = parse_local rest1 in
    let rest3 = expect (Lexer.To) rest2 in
    let (type_name, rest4) = parse_type rest3 in
      (RhsCast (local_name, type_name), rest4)

  | (Lexer.Un, _, _) :: _ -> 
    let (op, rest1) = parse_unop tokens in
    let (local_name, rest2) = parse_local rest1 in
      (RhsUn (op, local_name), rest2)

  | (Lexer.Bin, _, _) :: _ -> 
    let (op, rest1) = parse_binop tokens in
    let (local_name1, rest2) = parse_local rest1 in
    let rest3 = expect Lexer.Comma rest2 in
    let (local_name2, rest4) = parse_local rest3 in
      (RhsBin (op, local_name1, local_name2), rest4)

  | (Lexer.Addr_Of, _, _) :: rest1 -> 
    let (local_name, rest2) = parse_local rest1 in
      (RhsAddrOf local_name, rest2)

  | (Lexer.Member_Ptr, _, _) :: rest1 -> 
    let (local_name, rest2) = parse_local rest1 in
    let rest3 = expect Lexer.Comma rest2 in
    let (ident, rest4) = parse_ident rest3 in
      (RhsMemberPtr (local_name, ident), rest4)

  | (Lexer.Load, _, _) :: rest1 ->
    let (local_name, rest2) = parse_local rest1 in
      (RhsLoad local_name, rest2)

  | (Lexer.Store, _, _) :: rest1 -> 
    let (local_name1, rest2) = parse_local rest1 in
    let rest3 = expect Lexer.Comma rest2 in
    let (local_name2, rest4) = parse_local rest3 in
      (RhsStore (local_name1, local_name2), rest4)

  | (Lexer.Call, _, _) :: rest1 ->
    let (path, rest2) = parse_path rest1 in
    let rest3 = expect Lexer.LBracket rest2 in 
    let (args, rest4) = parse_args rest3 in
    let rest5 = expect Lexer.RBracket rest4 in
      (RhsCall (path, args), rest5)

  | (t, line, col) :: _ -> 
      failwith (Printf.sprintf "Syntax Error at Line %d, Col %d: Expected a RHS." line col)
  
  | [] -> failwith "Compiler Bug: Token stream empty without hitting EOF."
  
let parse_stmt tokens = 
  (* look for Local = , if missing just grab Rhs *)
  match tokens with
  | (Lexer.Local _, _, _) :: (Lexer.Equal, _, _) :: _ ->
    let (local, rest1) = parse_local tokens in
    let rest2 = expect Lexer.Equal rest1 in
    let (rhs, rest3) = parse_rhs rest2 in
      ({dest = Some local; rhs = rhs}, rest3)

  | (_, _, _) :: _ ->
    let (rhs, rest1) = parse_rhs tokens in
      ({dest = None; rhs = rhs}, rest1)

  | [] -> failwith "Compiler Bug: Token stream empty without hitting EOF."

let parse_term tokens = 
  match tokens with
  (* if jump consume label *)
  | (Lexer.Jump, _, _) :: rest1 ->
    let (label, rest2) = parse_label rest1 in
      (Jump label, rest2)

  (* if cjump consume local, label, label*)
  | (Lexer.Cjump, _, _) :: rest1 -> 
    let (local, rest2) = parse_local rest1 in
    let rest3 = expect Lexer.Comma rest2 in
    let (label1, rest4) = parse_label rest3 in
    let rest5 = expect Lexer.Comma rest4 in
    let (label2, rest6) = parse_label rest5 in
      (Cjump (local, label1, label2), rest6)

  (* if return followed by local, consume both *)
  | (Lexer.Return, _, _) :: (Lexer.Local _, _, _) :: _ ->
    let rest1 = expect Lexer.Return tokens in 
    let (local, rest2) = parse_local rest1 in
      (Return (Some local), rest2)

  (* if no local, return none *)
  | (Lexer.Return, _, _) :: rest1 ->
    (Return None, rest1)

  | (t, line, col) :: _ -> 
      failwith (Printf.sprintf "Syntax Error at Line %d, Col %d: Expected jump, cjump, or return." line col)

  | [] -> failwith "Compiler Bug: Token stream empty without hitting EOF."

let parse_block tokens = 
  (* consume label, loop over stmts, then final term *)
  let (label, rest1) = parse_label tokens in
  let rest2 = expect Lexer.Colon rest1 in

  let rec stmt_loop acc loop_tokens = 
      match loop_tokens with 
      | (Lexer.Jump, _, _) :: _ 
      | (Lexer.Cjump, _, _) :: _ 
      | (Lexer.Return, _, _) :: _ 
      | (Lexer.RCurly, _, _) :: _ -> 
          (List.rev acc, loop_tokens)
        
      | _ -> 
        let (next_stmt, loop_tokens1) = parse_stmt loop_tokens in
        let loop_tokens2 = expect Lexer.Semicolon loop_tokens1 in
        stmt_loop (next_stmt :: acc) loop_tokens2

    in

    let (stmts, rest3) = stmt_loop [] rest2 in
    let (term, rest4) = parse_term rest3 in
    let rest5 = expect Lexer.Semicolon rest4 in
      ({label = label; stmts = stmts; term = term}, rest5)

let parse_param tokens = 
  (* parsing Local, colon, Type *)
  let (local_name, rest1) = parse_local tokens in
  let rest2 = expect Lexer.Colon rest1 in
  let (type_name, rest3) = parse_type rest2 in
  ((local_name, type_name), rest3)

let parse_params tokens = 
(* grabbing one param, then looping over any others *)
match tokens with 
| (Lexer.RBracket, _, _) :: _ -> ([], tokens)
| _ ->
  let (first_param, rest1) = parse_param tokens in
    let rec params_loop acc loop_tokens =
      match loop_tokens with
      | (Lexer.Comma, _, _) :: loop_tokens1 ->
          let (new_param, loop_tokens2) = parse_param loop_tokens1 in
            params_loop (new_param :: acc) loop_tokens2
            
      | _ -> (List.rev acc, loop_tokens)

    in params_loop [first_param] rest1

let parse_rettype tokens = 
  (* catch void or resir_type *)
  match tokens with 
  | (Lexer.Void, _, _) :: rest -> 
    (Void, rest)
  | (_, _, _) :: _ ->
    let (ret_type, rest) = parse_type tokens in
    (RetType ret_type, rest)
  
  | [] -> failwith "Compiler Bug: Token stream empty without hitting EOF."
  
let parse_field tokens = 
  (* parsing identifier for name, then colon, then type *)
  let (field_name, rest1) = parse_ident tokens in
  let rest2 = expect Lexer.Colon rest1 in 
  let (field_type, rest3) = parse_type rest2 in
  ((field_name, field_type), rest3)
    
let parse_extern tokens =
  (* consuming Extern, Type, parsing path, then looping over Field; *)
  let rest1 = expect Lexer.Extern tokens in
  let rest2 = expect Lexer.Type rest1 in
  let (path_name, rest3) = parse_path rest2 in
  let rest4 = expect Lexer.LCurly rest3 in
    let rec field_loop acc loop_tokens =
      match loop_tokens with 
      | (Lexer.RCurly, _, _) :: _ -> (List.rev acc, loop_tokens)
      | _ -> 
          let (next_field, loop_tokens1) = parse_field loop_tokens in
          let loop_tokens2 = expect Lexer.Semicolon loop_tokens1 in
            field_loop (next_field :: acc) loop_tokens2
  in 
  let (fields, rest5) = field_loop [] rest4 in
  let rest6 = expect Lexer.RCurly rest5 in

  ({path = path_name; fields = fields }, rest6)

(* helper function for parse_function *)
let parse_fun_locals tokens = 
  (* slightly modified parse_params loop to grab extra semicolons *)
  match tokens with 
  | (Lexer.RCurly, _, _) :: _ -> ([], tokens)
  | _ ->
    let rec fun_local_loop acc loop_tokens =
      match loop_tokens with
      | (Lexer.Local _, _, _) :: _ ->
          let (new_local, loop_tokens1) = parse_local loop_tokens in
          let loop_tokens2 = expect Lexer.Colon loop_tokens1 in
          let (new_type, loop_tokens3) = parse_type loop_tokens2 in
          let loop_tokens4 = expect Lexer.Semicolon loop_tokens3 in
            fun_local_loop ((new_local, new_type) :: acc) loop_tokens4
      | _ -> (List.rev acc, loop_tokens)
    in fun_local_loop [] tokens

let parse_function tokens = 
  (* consume function keyword, path( *)
  match tokens with 
  | (Lexer.Function, _, _) :: rest1 ->
    let (path, rest2) = parse_path rest1 in
    let rest3 = expect Lexer.LBracket rest2 in

    (* grab params) -> rettype { locals { *)
    let (params, rest4) = parse_params rest3 in
    let rest5 = expect Lexer.RBracket rest4 in
    let rest6 = expect Lexer.Arrow rest5 in
    let (ret_type, rest7) = parse_rettype rest6 in
    let rest8 = expect Lexer.LCurly rest7 in 
    let rest9 = expect Lexer.Locals rest8 in
    let rest10 = expect Lexer.LCurly rest9 in

    (* grab all fun_locals } *)
    let (fun_locals, rest11) = parse_fun_locals rest10 in
    let rest12 = expect Lexer.RCurly rest11 in

    (* grab entry, label, ; *)
    let rest13 = expect Lexer.Entry rest12 in 
    let (label, rest14) = parse_label rest13 in
    let rest15 = expect Lexer.Semicolon rest14 in

    (* grab blocks and final } *)
    let (blocks, rest16) = (
      let rec block_loop acc loop_tokens = (
        match loop_tokens with
        | (Lexer.Label _, _, _) :: _ ->
          let (new_block, loop_tokens1) = parse_block loop_tokens in
            block_loop (new_block :: acc) loop_tokens1
        | _ -> (List.rev acc, loop_tokens)) in
      block_loop [] rest15 ) in

    let rest17 = expect Lexer.RCurly rest16 in

    ({path = path; 
      params = params; 
      ret_type = ret_type; 
      locals = fun_locals; 
      entry = label; 
      blocks = blocks}, rest17)

  | (t, line, col) :: _ -> 
      failwith (Printf.sprintf "Syntax Error at Line %d, Col %d: Expected a function keyword." line col)

  | [] -> failwith "Compiler Bug: Token stream empty without hitting EOF."

let parse_program tokens = 
  (* grab all externs then parse function *)
  let rec externs_loop acc loop_tokens = (
    match loop_tokens with 
    | (Lexer.Function, _, _) :: _ -> (List.rev acc, loop_tokens)
    | (Lexer.EOF, line, col) :: _ ->
      failwith (Printf.sprintf "Syntax Error at Line %d, Col %d: Expected a function definition." line col)
    | _ -> 
      let (new_extern, loop_tokens1) = parse_extern loop_tokens in
        externs_loop (new_extern :: acc) loop_tokens1 ) in

  let (externs, rest1) = externs_loop [] tokens in
  let (funct, rest2) = parse_function rest1 in
  let _ = expect Lexer.EOF rest2 in
    ({externs = externs; funct = funct}, rest2)