open Parser
open Type_checker

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

let extract_rhs_uses rhs =
  (* extract every local used in rhs *)
  match rhs with
  | RhsLocal loc            -> StringSet.singleton (string_of_local loc)
  | RhsConst _              -> StringSet.empty
  | RhsCast (loc, _)        -> StringSet.singleton (string_of_local loc)
  | RhsUn (_, loc)          -> StringSet.singleton (string_of_local loc)
  | RhsBin (_, loc1, loc2)  -> StringSet.of_list [string_of_local loc1; string_of_local loc2]
  | RhsAddrOf loc           -> StringSet.singleton (string_of_local loc)
  | RhsMemberPtr (loc, _)   -> StringSet.singleton (string_of_local loc)
  | RhsLoad loc             -> StringSet.singleton (string_of_local loc)
  | RhsStore (loc1, loc2)   -> StringSet.of_list [string_of_local loc1; string_of_local loc2]
  | RhsCall (_, locs)       -> StringSet.of_list (List.map string_of_local locs)

let extract_term_uses term =
  (* extract locals used in terminators *)
  match term with
  | Jump _            -> StringSet.empty
  | Cjump (loc, _, _) -> StringSet.singleton (string_of_local loc)
  | Return None       -> StringSet.empty
  | Return (Some loc) -> StringSet.singleton (string_of_local loc)

let extract_stmt_def dest =
  (* extract locals written to in statements *)
  match dest with
  | Some loc -> StringSet.singleton (string_of_local loc)
  | None     -> StringSet.empty

let block_gen_kill block =
  (* iterate over statements to build whole block generate and kill lists *)
  let rec walk stmts current_gen current_kill =
    match stmts with
    | [] -> (current_gen, current_kill)
    | stmt :: rest ->
      let uses = extract_rhs_uses stmt.rhs in
      let defs = extract_stmt_def stmt.dest in

      (* move variables generated in this statement from kill set to gen set *)
      let new_gen = StringSet.union current_gen (StringSet.diff uses current_kill) in

      (* move variables overwritten in this statement to kill set *)
      let new_kill = StringSet.union current_kill defs in

      walk rest new_gen new_kill
  in

  let (stmt_gen, stmt_kill) = walk block.stmts StringSet.empty StringSet.empty in
  let term_uses = extract_term_uses block.term in
  let final_gen = StringSet.union stmt_gen (StringSet.diff term_uses stmt_kill) in
    (final_gen, stmt_kill)

let get_block_successors block =
  (* identify successors *)
  match block.term with
    | Jump lbl -> [string_of_label lbl]
    | Cjump (_, lbl1, lbl2) -> [string_of_label lbl1; string_of_label lbl2]
    | Return _ -> []

let add_pred_edge lbl pred_map succ =
  (* finding all existing preds of succs, then appending current block to the list and updating map *)
  let current_preds = 
    match StringMap.find_opt succ pred_map with
    | Some l -> l
    | None   -> []
  in
    StringMap.add succ (lbl :: current_preds) pred_map

let process_block_cfg acc block =
  (* compute list of succs, then work add block to each succ's pred_map *)
  let (succ_acc, pred_acc) = acc in
  let lbl = string_of_label block.label in
  let succs = get_block_successors block in

  let new_succ_acc = StringMap.add lbl succs succ_acc in
  let new_pred_acc = List.fold_left (add_pred_edge lbl) pred_acc succs in
    (new_succ_acc, new_pred_acc)


let build_cfg_maps blocks =
  (* build a successor map and predecessor map for each block *)
  let empty_acc = (StringMap.empty, StringMap.empty) in
    List.fold_left process_block_cfg empty_acc blocks

let map_init_block acc block =
  (* initiate block within maps *)
  let (gen_acc, kill_acc, in_acc, out_acc, wl_acc) = acc in
  let lbl = string_of_label block.label in
  let (block_gen, block_kill) = block_gen_kill block in (
    StringMap.add lbl block_gen gen_acc,
    StringMap.add lbl block_kill kill_acc,
    StringMap.add lbl StringSet.empty in_acc,
    StringMap.add lbl StringSet.empty out_acc,
    lbl :: wl_acc )

let compute_liveness blocks =
  (* start by building all required maps to run worklist *)
  let (succ_map, pred_map) = build_cfg_maps blocks in

  let (gen_map, kill_map, in_map, out_map, initial_worklist) =
    List.fold_left map_init_block (StringMap.empty, StringMap.empty, StringMap.empty, StringMap.empty, []) blocks 
  
  in
  let rec worklist_loop wl current_in current_out =
    match wl with
    | [] -> (current_in, current_out) (* worklist is empty so we have reached the fixpoint *)

    | b :: rest_wl ->
      (* compute LiveOut[B] (the union of LiveIn[S] for each succ S) *)
      let succs =
        match StringMap.find_opt b succ_map with
        | Some l -> l
        | None   -> []
      in

      let new_out_b = List.fold_left (fun acc succ ->
        let in_succ = 
          match StringMap.find_opt succ current_in with 
          | Some s -> s 
          | None -> StringSet.empty in
          StringSet.union acc in_succ) StringSet.empty succs in
      
      (* compute LiveIn[B] (gen[B] U (LiveOut[B] \ kill[B])) *)
      let gen_b = StringMap.find b gen_map in
      let kill_b = StringMap.find b kill_map in
      let old_in_b = StringMap.find b current_in in

      let new_in_b = StringSet.union gen_b (StringSet.diff new_out_b kill_b) in

      let new_out = StringMap.add b new_out_b current_out in
      let new_in = StringMap.add b new_in_b current_in in

      (* fixpoint check *)
      if StringSet.equal old_in_b new_in_b then
        worklist_loop rest_wl new_in new_out
      
      else
        (* if In[B] changed re-evaluate non-duplicate predecessors *)
        let preds =
          match StringMap.find_opt b pred_map with
          | Some l -> l
          | None -> []
        in

        let next_wl = List.fold_left (fun acc p ->
          if List.mem p acc then acc else p :: acc ) rest_wl preds
        in

        worklist_loop next_wl new_in new_out
    in

    worklist_loop initial_worklist in_map out_map

(* moving onto dead code elimination *)
let safe_delete rhs =
  (* call and store have other in-memory side effects so they are never safe to delete *)
  match rhs with
  | RhsCall _ | RhsStore _ -> false
  | _ -> true

let optimise_block block live_out =
  (* add terminator uses manually *)
  let initial_live = StringSet.union live_out (extract_term_uses block.term) in

  (* walk through block statements checking if each is dead code *)
  let rec walk stmts current_live acc_stmts =
    match stmts with
    | [] -> acc_stmts
    | stmt :: rest ->
      let is_dead =
        match stmt.dest with
        | Some loc -> not (StringSet.mem (string_of_local loc) current_live)
        | None -> false
      in

      if is_dead && safe_delete stmt.rhs then
        (* drop stmt if it is dead and safe to delete *)
        walk rest current_live acc_stmts
      else
        (* if not dead or unsafe, remove dest from live set and add its uses to live set and keep it *)
        let new_live_without_def =
          match stmt.dest with
          | Some loc -> StringSet.remove (string_of_local loc) current_live
          | None -> current_live
        in
        let uses = extract_rhs_uses stmt.rhs in
        let new_live = StringSet.union new_live_without_def uses in
        walk rest new_live (stmt :: acc_stmts)
  in
  (* working bottom-up *)
  let optimised_stmts = walk (List.rev block.stmts) initial_live [] in
  {block with stmts = optimised_stmts}

(* clean up remaining empty locals *)
let extract_block_survivors block =
  let stmt_locals = List.fold_left (fun acc stmt ->
    let rhs_uses = extract_rhs_uses stmt.rhs in
    let defs = extract_stmt_def stmt.dest in
    StringSet.union acc (StringSet.union rhs_uses defs)) StringSet.empty block.stmts in

  StringSet.union stmt_locals (extract_term_uses block.term)

let extract_funct_survivors blocks =
  List.fold_left (fun acc block ->
    StringSet.union acc (extract_block_survivors block)) StringSet.empty blocks

let optimise_program (program : program * 'a) =
  (* optimise each blocks to make up funct of program *)
  let (stripped_program, rest) = program in
  let funct = stripped_program.funct in

  let (_in, out_map) = compute_liveness funct.blocks in
  let optimised_blocks =
    List.map (fun block ->
      let block_out = StringMap.find (string_of_label block.label) out_map in
      optimise_block block block_out) funct.blocks
  in

  let surviving_locals = extract_funct_survivors optimised_blocks in
  let optimised_locals = List.filter (fun (loc, _) ->
    StringSet.mem (string_of_local loc) surviving_locals) funct.locals in

  let optimised_funct = {funct with blocks = optimised_blocks; locals = optimised_locals} in
  ({stripped_program with funct = optimised_funct}, rest)