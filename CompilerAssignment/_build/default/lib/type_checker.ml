open Parser

type signature = {
  args : resir_type list;
  ret  : ret_type;
}

type environment = {
  extern_types     : (path * field list) list; (* struct name -> list of strings and types *)
  locals           : param list;               (* "%x" -> type *)
  labels           : label list;               (* list of valid block names "bb0", "bb1", ... *)
  funct            : funct;                    (* track the function being checked *)
  extern_functions : (path * signature) list;        (* list of function paths and signatures that are already defined to type check *)
}

let rec check_type_exists ext_types ret_typ =
  match ret_typ with
  | Void -> ()
  | RetType typ -> (
    match typ with
    | Prim _ -> ()
    | Ptr inner_typ ->
      check_type_exists ext_types (RetType inner_typ)
    | Path p ->
      if not (List.mem_assoc p ext_types) then
        failwith (Printf.sprintf "Semantic Error: Unknown custom type \"%s\" used." (string_of_path p)) )

let build_environment program =
  (* populate extern types *)
  let get_extern_types =
    List.map (fun (ext : extern_type) -> (ext.path, ext.fields)) program.externs in
  let ext_types = get_extern_types in

  (* check for duplicate externs *)
  let extern_paths = List.map fst get_extern_types in
  let unique_extern_paths = List.sort_uniq compare extern_paths in
  if List.length extern_paths <> List.length unique_extern_paths then
    failwith "Semantic Error: Duplicate extern type declarations.";

  (* group params and locals then check they're all unique *)
  let all_locals = program.funct.params @ program.funct.locals in
  let local_names = List.map fst all_locals in
  let unique_local_names = List.sort_uniq compare local_names in
  if List.length local_names <> List.length unique_local_names then
    failwith "Semantic Error: Duplicate local variable or parameter declarations.";

  (* check if all labels are unique *)
  let labels_list = List.map (fun b -> b.label) program.funct.blocks in
  let unique_labels = List.sort_uniq compare labels_list in
  if List.length labels_list <> List.length unique_labels then
    failwith "Semantic Error: Block labels must be unique.";

  (* check if entry actually exists *)
  if not (List.mem program.funct.entry labels_list) then (
    let Label entry_str = program.funct.entry in
    failwith (Printf.sprintf "Semantic Error: Declared entry block %s does not exist." entry_str));

  (* check if all custom types were declared, first in each extern, then return type, then locals *)
  List.iter (fun (ext : extern_type) -> 
    List.iter (fun (_, typ) -> check_type_exists ext_types (RetType typ)) ext.fields) program.externs;

  check_type_exists ext_types program.funct.ret_type;

  List.iter (fun (_, typ) -> check_type_exists ext_types (RetType typ)) all_locals;

  (* hardcode known functions *)
  let known_externs = [
    (["Math"; "abs_diff"], {args = [Prim I32; Prim I32]; ret = RetType (Prim I32)})
  ] in

  {
    extern_types = ext_types;
    locals = all_locals;
    labels = labels_list;
    funct = program.funct;
    extern_functions = known_externs;
  }

let search_local env local =
  try 
    List.assoc local env.locals (* search for the associated resir_type *)
  with Not_found ->
    let (Local name) = local in
      failwith (Printf.sprintf "Semantic Error: Use of undeclared variable %s." name)

let search_extern env path =
  try
    List.assoc path env.extern_types
  with Not_found ->
    failwith (Printf.sprintf "Semantic Error: Unknown extern path \"%s\"." (string_of_path path))

let search_field fields target_field =
  try 
    List.assoc target_field fields
  with Not_found ->
    failwith (Printf.sprintf "Semantic Error: Extern does not have field \"%s\"." target_field)
    
let is_valid_cast src_type target_type =
  (* based on C allowed casts, int to int, ptr to ptr, int to ptr, and ptr to int*)
  match src_type, target_type with
  | Prim _, Prim _ -> true
  | Ptr _, Ptr _ -> true
  | Prim (I32 | I64 | U32), Ptr _ -> true
  | Ptr _, Prim (I32 | I64 | U32) -> true
  | _ -> false

let is_valid_dest env dest rhs_type =
  match dest, rhs_type with
  | Some dest_local, Some rhs_type ->
    let dest_type = search_local env dest_local in
    if dest_type <> rhs_type then
      failwith (Printf.sprintf "Semantic Error: Type mismatch. Cannot assign type %s to variable %s of type %s." (string_of_resir_type rhs_type) (string_of_local dest_local) (string_of_resir_type dest_type))

  | None, _ -> () (* void operation with no destination or discarding a produced value *)

  | Some dest_local, None ->
    failwith (Printf.sprintf "Semantic Error: Cannot assign a void expression to variable %s." (string_of_local dest_local))

  (* | None, Some rhs_type ->
    failwith (Printf.sprintf "Semantic Error: Expression produces a value of type %s but no destination variable was provided." (string_of_resir_type rhs_type)) *)

let get_rhs_type env expr =
  match expr with
  | RhsLocal local ->
      Some (search_local env local)

  | RhsCast (local, target_type) ->
    let src_type = search_local env local in
    check_type_exists env.extern_types (RetType target_type);
      if is_valid_cast src_type target_type then Some target_type
      else failwith (Printf.sprintf "Semantic Error: Invalid type cast. %s cannot be cast to %s." (string_of_resir_type src_type) (string_of_resir_type target_type))

  | RhsUn (op, local) ->
    let typ = search_local env local in (
    match op, typ with
    | Neg, Prim Bool ->
      failwith "Semantic Error: Cannot apply Neg to local of type Bool. Did you mean 'not'?"
    | Neg, Prim _ -> Some typ
    
    | Not, Prim Bool -> Some typ
    | Not, Prim t ->
      failwith (Printf.sprintf "Semantic Error: Cannot apply Not to local of type %s. Did you mean 'neg'?" (string_of_prim_type t))
    
    | op, Path p ->
      failwith (Printf.sprintf "Semantic Error: Cannot apply %s to custom type %s." (string_of_unop op) (string_of_path p))
    | op, Ptr p ->
      failwith (Printf.sprintf "Semantic Error: Cannot apply %s to pointer %s." (string_of_unop op) (string_of_resir_type p)) )

  | RhsBin (op, local1, local2) -> (
    let typ1 = search_local env local1 in
    let typ2 = search_local env local2 in

    (* C pointer arithmetic *)
    match op, typ1, typ2 with
    | (Add | Sub), Ptr p, Prim (I32 | I64 | U32) -> Some (Ptr p)
    | Add, Prim (I32 | I64 | U32), Ptr p -> Some (Ptr p) 
    | Sub, Ptr p1, Ptr p2 when p1 = p2 -> Some (Prim I64)
    | _ -> (

      if typ1 <> typ2 then 
        failwith (Printf.sprintf "Semantic Error: Operand type mismatch in %s operation." (string_of_binop op));
      match op, typ1 with
      (* arithmetic for numbers *)
      | (Add | Sub | Mul | Div | Mod), Prim Bool ->
        failwith (Printf.sprintf "Semantic Error: Cannot apply %s to locals of type Bool." (string_of_binop op))
      | (Add | Sub | Mul | Div), Prim _ -> Some typ1
      | Mod, Prim F64 -> failwith "Semantic Error: Mod is not valid for F64."
      | Mod, Prim _ -> Some typ1

      (* equality of numbers, bools, and ptrs *)
      | (Eq | Ne), Prim _ -> Some (Prim Bool)
      | (Eq | Ne), Ptr _ -> Some (Prim Bool)

      (* relational operators for numbers only *)
      | (Lt | Le | Gt | Ge), Prim Bool ->
        failwith (Printf.sprintf "Semantic Error: Cannot apply %s to locals of type Bool." (string_of_binop op))
      | (Lt | Le | Gt | Ge), Prim _ -> Some (Prim Bool)

      (* logical operators for boolean only *)
      | (And | Or), Prim Bool -> Some (Prim Bool)
      | (And | Or), Prim p -> 
        failwith (Printf.sprintf "Semantic Error: Cannot apply %s to local of type %s." (string_of_binop op) (string_of_prim_type p))

      (* errors on paths and pointers *)
      | op, Path p ->
        failwith (Printf.sprintf "Semantic Error: Cannot apply %s to custom type %s." (string_of_binop op) (string_of_path p))
      | op, Ptr p ->
        failwith (Printf.sprintf "Semantic Error: Cannot apply %s to pointer %s. Only equality checks are allowed." (string_of_binop op) (string_of_resir_type p)) ) )

  | RhsAddrOf local ->
    Some (Ptr (search_local env local))

  | RhsMemberPtr (local, name) ->
    (match search_local env local with
    | Ptr (Path p) -> 
      let fields = search_extern env p in
      let field_type = search_field fields name in
      Some (Ptr field_type)

    | _ ->
      failwith "Semantic Error: member_ptr requires a pointer to an extern path."
    )

  | RhsLoad ptr_local ->
      (match search_local env ptr_local with
       | Ptr inner_type -> Some inner_type
       | _ -> failwith "Semantic Error: Cannot load from a non-pointer type.")

  | RhsStore (ptr, value) ->
    let ptr_type = search_local env ptr in
    let value_type = search_local env value in (
    match ptr_type with
    | Ptr inner_type when inner_type = value_type -> None (* this is a void operation *)
    | Ptr _ -> failwith "Semantic Error: Store destination pointer type must match value type."
    | _ -> failwith "Semantic Error: First argument of store must be a pointer type."
    )

  | _ -> None

let check_term env term =
  match term with
  | Jump target_label ->
    if not (List.mem target_label env.labels) then
      failwith (Printf.sprintf "Semantic Error: Target of jump \"%s\" does not exist." (string_of_label target_label))
  
  | Cjump (condition, label_true, label_false) ->
    if (search_local env condition) <> Prim Bool then
      failwith (Printf.sprintf "Semantic Error: cjump condition \"%s\" is not of Bool type." (string_of_local condition));

    if not (List.mem label_true env.labels) then
      failwith (Printf.sprintf "Semantic Error: Target of cjump \"%s\" does not exist." (string_of_label label_true));

    if not (List.mem label_false env.labels) then
      failwith (Printf.sprintf "Semantic Error: Target of cjump \"%s\" does not exist." (string_of_label label_false))

  | Return local_opt ->
    match env.funct.ret_type, local_opt with
    | Void, None -> ()
    | RetType expected_type, Some ret_local ->
        let actual_type = search_local env ret_local in
        if actual_type <> expected_type then
          failwith (Printf.sprintf "Semantic Error: Return type mismatch. Expected %s but found %s." (string_of_resir_type expected_type) (string_of_resir_type actual_type))
    | Void, Some _ ->
      failwith "Semantic Error: Void functions cannot return a value."
    | RetType t, None ->
      failwith (Printf.sprintf "Semantic Error: Function must return a value of type %s." (string_of_resir_type t))

let check_statement env stmt =
  match stmt.dest, stmt.rhs with
  (* constant literals *)
  | Some dest_local, RhsConst lit ->
    let dest_type = search_local env dest_local in (
    match lit, dest_type with
    | BoolLit _, Prim Bool -> ()
    | IntLit _, Prim (I32 | I64 | U32) -> ()
    | FloatLit _, Prim F64 -> ()
    | NullLit, Ptr _ -> ()
    | _ -> failwith (Printf.sprintf "Semantic Error: Constant literal \"%s\" does not match destination type %s." (string_of_literal lit) (string_of_resir_type dest_type))
    )
  
  | None, RhsConst lit -> 
    failwith (Printf.sprintf "Semantic Error: Constant literal \"%s\" cannot exist without a destination variable." (string_of_literal lit))

  | dest_opt, RhsCall (funct_path, args) ->
      let sig_opt =
        if funct_path = env.funct.path then
          (* recursive call signature *)
          Some {args = List.map snd env.funct.params; ret = env.funct.ret_type}

        else
          (* lookup external function *)
          List.assoc_opt funct_path env.extern_functions
      in
      (match sig_opt with
      (* found matching signature, either recursive or pre-existing, so compare signature with vars passed to it *)
      | Some callee_sig ->
          let arg_types = List.map (search_local env) args in

          if List.length arg_types <> List.length callee_sig.args then
            failwith (Printf.sprintf "Semantic Error: Arity mismatch in call to \"%s\"." (string_of_path funct_path));

          if arg_types <> callee_sig.args then
            failwith (Printf.sprintf "Semantic Error: Argument type mismatch in call to \"%s\"." (string_of_path funct_path));

          let rhs_type_opt = match callee_sig.ret with Void -> None | RetType t -> Some t in
          is_valid_dest env dest_opt rhs_type_opt

      | None ->
        (* call to an undefined function, first checking that args exist, then checking that dest_local exists *)
        List.iter (fun arg -> ignore (search_local env arg)) args;
        match dest_opt with 
        | Some dest_local -> ignore (search_local env dest_local)
        | None -> ()  )

  | dest_opt, rhs_expr ->
    let rhs_type_opt = get_rhs_type env rhs_expr in
    is_valid_dest env dest_opt rhs_type_opt

let check_block env block =
  List.iter (check_statement env) block.stmts;
  check_term env block.term

let validate_program program =
  let (stripped_program, _) = program in
  let env = build_environment stripped_program in
  List.iter (check_block env) stripped_program.funct.blocks;
  (* if the program has not crash we're all good! *)