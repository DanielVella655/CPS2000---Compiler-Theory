open CompilerAssignment
open Types
open Lexer

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

let string_of_tokens token =
  match token with 
  | (text, _, _) -> (
    match text with
    | Extern -> "Extern" | Type -> "Type" | Function -> "Function" | Locals -> "Locals" 
    | Void -> "Void" | Entry -> "Entry" | To -> "To" | Const -> "Const" | Cast -> "Cast" 
    | Un -> "Un" | Bin -> "Bin" | Addr_Of -> "Addr_Of" | Member_Ptr -> "Member_Ptr" 
    | Load -> "Load" | Store -> "Store" | Call -> "Call" | Jump -> "Jump" | Cjump -> "Cjump" 
    | Return -> "Return" | Bool -> "Bool" | I32 -> "I32" | I64 -> "I64" | U32 -> "U32" | F64 -> "F64" | Ptr -> "Ptr" 
    | Ident s -> "Ident(" ^ s ^ ")" 
    | Local s -> "Local(" ^ s ^ ")" 
    | Label s -> "Label(" ^ s ^ ")" 
    | IntLit i -> "IntLit(" ^ string_of_int i ^ ")" 
    | FloatLit f -> "FloatLit(" ^ string_of_float f ^ ")" 
    | True -> "True" | False -> "False" | Null -> "Null" 
    | Neg -> "Neg" | Not -> "Not" | Add -> "Add" | Sub -> "Sub" | Mul -> "Mul" | Div -> "Div" | Mod -> "Mod" 
    | Eq -> "Eq" | Ne -> "Ne" | Lt -> "Lt" | Le -> "Le" | Gt -> "Gt" | Ge -> "Ge" 
    | And -> "And" | Or -> "Or" 
    | LBracket -> "LBracket" | RBracket -> "RBracket" | LCurly -> "LCurly" | RCurly -> "RCurly" 
    | LAngle -> "LAngle" | RAngle -> "RAngle" | Colon -> "Colon" | Semicolon -> "Semicolon" 
    | PathSep -> "PathSep" | Arrow -> "Arrow" | Equal -> "Equal" | Comma -> "Comma" 
    | EOF -> "EOF"
  )

let run_test_case resir_path expected_path =
  let input_code = read_file resir_path in
  let expected_output = String.trim (read_file expected_path) in
  let tokens = Lexer.tokenize input_code in
  let token_string = List.map string_of_tokens tokens in
  let actual_output = String.trim (String.concat "\n" token_string) in

  if actual_output = expected_output then (
    Printf.printf "  [PASS] %s\n" resir_path
  )
  else (
    Printf.printf "  [FAIL] %s\n" resir_path;
    Printf.printf "    Expected: %s\n" expected_output;
    Printf.printf "    Got:      %s\n" actual_output;
    exit 1
  )

let run_error_test_case resir_path expected_path = 
  let input_code = read_file resir_path in
  let expected_error = String.trim (read_file expected_path) in
  try
    let _ = Lexer.tokenize input_code in
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
  print_endline "Running ResIR Compiler Test Suite...";
  
  for x = 1 to 2 do (run_test_case ("test/test_cases/lexer_tests/test" ^ string_of_int x ^ ".resir") ("test/test_cases/lexer_tests/test" ^ string_of_int x ^ ".expected")) done;
  for x = 3 to 5 do (run_error_test_case ("test/test_cases/lexer_tests/test" ^ string_of_int x ^ "_err.resir") ("test/test_cases/lexer_tests/test" ^ string_of_int x ^ "_err.expected")) done;
  
  print_endline "All tests passed successfully!"