open CompilerAssignment
open Code_generator

let () =
  (* check if a filename was provided *)
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Usage: dune exec -- ./main.exe <filename.resir>\n";
    exit 1
  );

  let input_file = Sys.argv.(1) in

  (* replace .resir with .c *)
  let output_file =
    if Filename.check_suffix input_file ".resir" then
      (Filename.chop_suffix input_file ".resir") ^ ".c"
    else
      input_file ^ ".c"
  in

  (* read the entire input file into a string *)
  let source_code =
    try
      let ic = open_in input_file in
      let n = in_channel_length ic in
      let s = really_input_string ic n in
      close_in ic;
      s
    with Sys_error msg ->
      Printf.eprintf "Error reading file: %s\n" msg;
      exit 1
  in

  (* compile the source code *)
  try
    let c_code = Code_generator.gen_program_opt source_code in

    let oc = open_out output_file in
    output_string oc c_code;
    output_string oc "\n";
    close_out oc;

    Printf.printf "Successfully compiled '%s' to '%s'\n" input_file output_file

  with
  | Failure err_msg ->
      (* catches the failwith errors *)
      Printf.eprintf "%s\n" err_msg;
      exit 1
  | exn ->
      (* catch-all for any other unexpected OCaml exceptions *)
      Printf.eprintf "Compiler Bug/Fatal Error: %s\n" (Printexc.to_string exn);
      exit 1