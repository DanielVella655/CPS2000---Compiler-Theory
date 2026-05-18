open CompilerAssignment

(* Helper to read a file's content into a string *)
let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

(* A placeholder test execution function *)
let run_test_case resir_path expected_path =
  let input_code = read_file resir_path in
  let expected_output = String.trim (read_file expected_path) in
  
  (* PHASE 1: Right now, this just passes the code to your Lexer stringifier *)
  (* e.g., let actual_output = Lexer.tokenize_to_string input_code *)
  let actual_output = String.trim input_code in (* Placeholder *)

  if actual_output = expected_output then
    Printf.printf "  [PASS] %s\n" resir_path
  else (
    Printf.printf "  [FAIL] %s\n" resir_path;
    Printf.printf "    Expected: %s\n" expected_output;
    Printf.printf "    Got:      %s\n" actual_output;
    exit 1
  )

let () =
  print_endline "Running ResIR Compiler Test Suite...";
  
  (* You can automate this later by reading the directory dynamically *)
  run_test_case "success_cases/math_diff.resir" "success_cases/math_diff.expected";
  
  print_endline "All tests passed successfully!"