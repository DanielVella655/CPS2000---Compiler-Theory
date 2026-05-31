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

let compute_liveness blocks =
  let (succ_map, pred_map) = build_cfg_maps blocks in

  