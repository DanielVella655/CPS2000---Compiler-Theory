open CompilerAssignment
open Parser

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

(* resolving path to avoid errors between dune exec and dune test*)
let resolve_path path =
  if Sys.file_exists path then path
  else if Sys.file_exists ("test/" ^ path) then "test/" ^ path
  else
    let len = String.length path in
    if len > 5 && String.sub path 0 5 = "test/" then
      let stripped = String.sub path 5 (len - 5) in
      if Sys.file_exists stripped then stripped else path
    else path

(*  reading a file's content into a string *)
let read_file filename =
  let safe_path = resolve_path filename in
  let ch = open_in safe_path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let print_file filename content = 
  let safe_path = resolve_path filename in
  let ch = open_out safe_path in
  output_string ch content;
  close_out ch

let run_test_case resir_path expected_path =
  (* round trip testing *)
  let input_code = read_file resir_path in
  let expected_output = String.trim (read_file expected_path) in
  let tokens = Lexer.tokenize input_code in
  let (program, _) = Parser.parse_program tokens in
  let parsed_string = print_parsed program in
    
    Printf.printf "  Successfully parsed once.\n";
    let tokens2 = Lexer.tokenize parsed_string in
    let (program2, _) = Parser.parse_program tokens2 in
    let parsed_string2 = print_parsed program2 in

    if parsed_string = parsed_string2 then (
      Printf.printf "  [PASS] %s\n" resir_path
    )
    else (
      Printf.printf "  [FAIL] %s\n" resir_path;
      Printf.printf "    Expected: %s\n" expected_output;
      Printf.printf "    Got:      %s\n" parsed_string2;
      exit 1
    )

let run_error_test_case resir_path expected_path = 
  let input_code = read_file resir_path in
  let expected_error = String.trim (read_file expected_path) in
  let tokens = Lexer.tokenize input_code in
  try
    let _ = Parser.parse_program tokens in
      Printf.printf "  [FAIL] %s (Expected error but test passed)\n" resir_path;
      exit 1
  with Failure actual_error->
    if String.trim actual_error = String.trim expected_error then
      Printf.printf "  [PASS] %s (Caught expected exception: \"%s\")\n" resir_path actual_error
    else (
      Printf.printf "  [FAIL] %s (Exception message mismatch)\n" resir_path;
      Printf.printf "    Expected error: %s\n" expected_error;
      Printf.printf "    Got error:      %s\n" actual_error;
      exit 1
    )

let () =
  print_endline "Running ResIR Parser Test Suite...";
  
  for x = 1 to 2 do (run_test_case ("test/test_cases/parser_tests/test" ^ string_of_int x ^ ".resir") ("test/test_cases/parser_tests/test" ^ string_of_int x ^ ".expected")) done;
  for x = 3 to 5 do (run_error_test_case ("test/test_cases/parser_tests/test" ^ string_of_int x ^ "_err.resir") ("test/test_cases/parser_tests/test" ^ string_of_int x ^ "_err.expected")) done;
  
  print_endline "All tests passed successfully!"