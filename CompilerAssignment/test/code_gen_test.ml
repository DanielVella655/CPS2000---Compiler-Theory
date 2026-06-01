open CompilerAssignment
open Code_generator

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
  let program = Parser.parse_program tokens in
    Type_checker.validate_program program;
    let c_code = String.trim (Code_generator.gen_program program) in

      if c_code = expected_output then (
        Printf.printf "  [PASS] %s\n" resir_path
      )
      else (
        Printf.printf "  [FAIL] %s\n" resir_path;
        Printf.printf "    Expected: %s\n" expected_output;
        Printf.printf "    Got:      %s\n" c_code;
        exit 1
      )

let () =
  print_endline "Running ResIR Code Generator Test Suite...";
  
  for x = 1 to 4 do (run_test_case ("test/test_cases/code_gen_tests/test" ^ string_of_int x ^ ".resir") ("test/test_cases/code_gen_tests/test" ^ string_of_int x ^ ".expected.c")) done;
  
  print_endline "All tests passed successfully!\n"