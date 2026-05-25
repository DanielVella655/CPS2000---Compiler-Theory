type token = 
(* Structure and Declaration *)
Extern | Type | Function | Locals | Void | Entry | To |
(* Instruction *)
Const | Cast | Un | Bin | Addr_Of | Member_Ptr | Load | Store | Call | Jump | Cjump | Return |
(* Types *)
Bool | I32 | I64 | U32 | F64 | Ptr | 
(* Labels and Identifiers *)
Ident of string | 
Local of string | (* ex: %a *)
Label of string |
(* Literals *)
IntLit of int | FloatLit of float | True | False | Null |
(* Operators *)
Neg | Not |
Add | Sub | Mul | Div | Mod | 
Eq | Ne | Lt | Le | Gt | Ge | 
And | Or |
(* Punctuation *)
LBracket | RBracket | LCurly | RCurly | LAngle | RAngle | Colon | Semicolon | PathSep | Arrow | Equal | Comma | (* where PathSep is :: *)
EOF

(* helper function for printing errors *)
let string_of_tokens token =
  match token with 
  | (Extern, line, col)     -> "(Extern, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Type, line, col)       -> "(Type, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Function, line, col)   -> "(Function, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Locals, line, col)     -> "(Locals, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")" 
  | (Void, line, col)       -> "(Void, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Entry, line, col)      -> "(Entry, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (To, line, col)         -> "(To, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Const, line, col)      -> "(Const, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Cast, line, col)       -> "(Cast, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")" 
  | (Un, line, col)         -> "(Un, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Bin, line, col)        -> "(Bin, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Addr_Of, line, col)    -> "(Addr_Of, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Member_Ptr, line, col) -> "(Member_Ptr, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")" 
  | (Load, line, col)       -> "(Load, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Store, line, col)      -> "(Store, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Call, line, col)       -> "(Call, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Jump, line, col)       -> "(Jump, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Cjump, line, col)      -> "(Cjump, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")" 
  | (Return, line, col)     -> "(Return, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Bool, line, col)       -> "(Bool, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (I32, line, col)        -> "(I32, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (I64, line, col)        -> "(I64, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (U32, line, col)        -> "(U32, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (F64, line, col)        -> "(F64, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Ptr, line, col)        -> "(Ptr, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")" 
  | (Ident s, line, col)    -> "(Ident(" ^ s ^ "), " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")" 
  | (Local s, line, col)    -> "(Local(" ^ s ^ "), " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")" 
  | (Label s, line, col)    -> "(Label(" ^ s ^ "), " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")" 
  | (IntLit i, line, col)   -> "(IntLit(" ^ string_of_int i ^ "), " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")" 
  | (FloatLit f, line, col) -> "(FloatLit(" ^ string_of_float f ^ "), " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")" 
  | (True, line, col)       -> "(True, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (False, line, col)      -> "(False, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Null, line, col)       -> "(Null, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")" 
  | (Neg, line, col)        -> "(Neg, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Not, line, col)        -> "(Not, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Add, line, col)        -> "(Add, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Sub, line, col)        -> "(Sub, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Mul, line, col)        -> "(Mul, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Div, line, col)        -> "(Div, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Mod, line, col)        -> "(Mod, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")" 
  | (Eq, line, col)         -> "(Eq, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Ne, line, col)         -> "(Ne, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Lt, line, col)         -> "(Lt, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Le, line, col)         -> "(Le, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Gt, line, col)         -> "(Gt, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Ge, line, col)         -> "(Ge, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")" 
  | (And, line, col)        -> "(And, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Or, line, col)         -> "(Or, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")" 
  | (LBracket, line, col)   -> "(LBracket, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (RBracket, line, col)   -> "(RBracket, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (LCurly, line, col)     -> "(LCurly, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (RCurly, line, col)     -> "(RCurly, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")" 
  | (LAngle, line, col)     -> "(LAngle, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (RAngle, line, col)     -> "(RAngle, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Colon, line, col)      -> "(Colon, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Semicolon, line, col)  -> "(Semicolon, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")" 
  | (PathSep, line, col)    -> "(PathSep, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Arrow, line, col)      -> "(Arrow, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Equal, line, col)      -> "(Equal, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"
  | (Comma, line, col)      -> "(Comma, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")" 
  | (EOF, line, col)        -> "(EOF, " ^ string_of_int line ^ ", " ^ string_of_int col ^ ")"

let map_keywords s = 
    match s with
    | "extern" -> Extern | "type" -> Type | "function" -> Function | "locals" -> Locals | "void" -> Void | "entry" -> Entry | "to" -> To 
    | "const" -> Const | "cast" -> Cast | "un" -> Un | "bin" -> Bin | "addr_of" -> Addr_Of | "member_ptr" -> Member_Ptr | "load" -> Load | "store" -> Store | "call" -> Call 
        | "jump" -> Jump | "cjump" -> Cjump | "return" -> Return 
    | "bool" -> Bool | "i32" -> I32 | "i64" -> I64 | "u32" -> U32 | "f64" -> F64 | "ptr" -> Ptr
    | "true" -> True | "false" -> False | "null" -> Null 
    | _ -> Ident s 

type chars = 
| Alpha | Digit | Percent | Colon | Plus | Minus | Equal | Greater | Period | Punct | Space | Other

let map_char c = 
    match c with 
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> Alpha 
    | '0' .. '9' -> Digit
    | '%' -> Percent 
    | ':'  -> Colon
    | '+' -> Plus
    | '-' -> Minus
    | '=' -> Equal
    | '>' -> Greater 
    | '.' -> Period
    | ';' | ',' | '(' | ')' | '{' | '}' | '<' -> Punct
    | ' ' | '\t' | '\n' | '\r' -> Space 
    | _ -> Other

type states = 
| StartSt | IdentSt | IntLitSt | FloatLitSt | PercentSt | LocalSt | ColonSt | PathSepSt | PlusSt | MinusSt 
| ArrowSt | EqualSt | GreaterSt | PunctSt | ErrorSt

let transition_table = [|
    (* Alpha  | Digit   |   Percent  | Colon  |   Plus  |  Minus  | Equal  | Greater  | Period |    Punct  | Space  | Other *)
    (* Start state *)
    [| IdentSt; IntLitSt;   PercentSt; ColonSt;   PlusSt;  MinusSt; EqualSt; GreaterSt; ErrorSt;    PunctSt; ErrorSt; ErrorSt|];
    (* Ident state *)
    [| IdentSt; IdentSt;    ErrorSt;   ErrorSt;   ErrorSt; ErrorSt; ErrorSt; ErrorSt;   ErrorSt;    ErrorSt; ErrorSt; ErrorSt|];
    (* IntLit state *)
    [| ErrorSt; IntLitSt;   ErrorSt;   ErrorSt;   ErrorSt; ErrorSt; ErrorSt; ErrorSt;   FloatLitSt; ErrorSt; ErrorSt; ErrorSt|];
    (* FloatLit state *)
    [| ErrorSt; FloatLitSt; ErrorSt;   ErrorSt;   ErrorSt; ErrorSt; ErrorSt; ErrorSt;   ErrorSt;    ErrorSt; ErrorSt; ErrorSt|];
    (* Percent state *)
    [| LocalSt; ErrorSt;    ErrorSt;   ErrorSt;   ErrorSt; ErrorSt; ErrorSt; ErrorSt;   ErrorSt;    ErrorSt; ErrorSt; ErrorSt|];
    (* Local state *)
    [| LocalSt; LocalSt;    ErrorSt;   ErrorSt;   ErrorSt; ErrorSt; ErrorSt; ErrorSt;   ErrorSt;    ErrorSt; ErrorSt; ErrorSt|];
    (* Colon state *)
    [| ErrorSt; ErrorSt;    ErrorSt;   PathSepSt; ErrorSt; ErrorSt; ErrorSt; ErrorSt;   ErrorSt;    ErrorSt; ErrorSt; ErrorSt|];
    (* PathSep state *)
    [| ErrorSt; ErrorSt;    ErrorSt;   ErrorSt;   ErrorSt; ErrorSt; ErrorSt; ErrorSt;   ErrorSt;    ErrorSt; ErrorSt; ErrorSt|];
    (* Plus state *)
    [| ErrorSt; IntLitSt;   ErrorSt;   ErrorSt;   ErrorSt; ErrorSt; ErrorSt; ErrorSt;   ErrorSt;    ErrorSt; ErrorSt; ErrorSt|];
    (* Minus state *)
    [| ErrorSt; IntLitSt;   ErrorSt;   ErrorSt;   ErrorSt; ErrorSt; ErrorSt; ArrowSt;   ErrorSt;    ErrorSt; ErrorSt; ErrorSt|];
    (* Arrow state *)
    [| ErrorSt; ErrorSt;    ErrorSt;   ErrorSt;   ErrorSt; ErrorSt; ErrorSt; ErrorSt;   ErrorSt;    ErrorSt; ErrorSt; ErrorSt|];
    (* Equal state *)
    [| ErrorSt; ErrorSt;    ErrorSt;   ErrorSt;   ErrorSt; ErrorSt; ErrorSt; ErrorSt;   ErrorSt;    ErrorSt; ErrorSt; ErrorSt|];
    (* Greater state *)
    [| ErrorSt; ErrorSt;    ErrorSt;   ErrorSt;   ErrorSt; ErrorSt; ErrorSt; ErrorSt;   ErrorSt;    ErrorSt; ErrorSt; ErrorSt|];
    (* Punct state *)
    [| ErrorSt; ErrorSt;    ErrorSt;   ErrorSt;   ErrorSt; ErrorSt; ErrorSt; ErrorSt;   ErrorSt;    ErrorSt; ErrorSt; ErrorSt|];
|]

let is_numeric s = (* check whether each char is numeric *)
    let len = String.length s in
        if len = 0 then false
        else
            let rec loop i =
                if i = len then true
                else if s.[i] >= '0' && s.[i] <= '9' then loop (i+1)
                else false 
            in loop 0

let is_label s = (* check whether string is of the form 'bb' Digit {Digit} *)
    if String.length s >= 2 && String.sub s 0 2 = "bb" then 
        is_numeric (String.sub s 2 (String.length s - 2))
    else false

let map_states state lexeme line col = 
    let err_prefix = Printf.sprintf "Lexical Error at Line %d, Col %d: " line col in
    match state with 
    | IdentSt -> if is_label lexeme then Label lexeme else map_keywords lexeme
    | IntLitSt -> IntLit (int_of_string lexeme)
    | FloatLitSt -> FloatLit (float_of_string lexeme)
    | LocalSt -> Local lexeme
    | ColonSt -> Colon
    | PathSepSt -> PathSep
    | ArrowSt -> Arrow
    | EqualSt -> Equal
    | GreaterSt -> RAngle
    | PunctSt -> 
        (match lexeme.[0] with
        | ';' -> Semicolon | ',' -> Comma | '(' -> LBracket | ')' -> RBracket | '{' -> LCurly | '}' -> RCurly | '<' -> LAngle
        | _ -> failwith (err_prefix ^ "Unrecognised Punctuation: '" ^ lexeme ^ "' ."))
    (* incomplete token states *)
    | StartSt -> failwith (err_prefix ^ "Empty Token")
    | PercentSt -> failwith (err_prefix ^ "Standalone '%' symbol. Expected an identifier.")
    | PlusSt -> failwith (err_prefix ^ "Standalone '+' symbol. Expected a literal. Did you mean 'add'?")
    | MinusSt -> failwith (err_prefix ^ "Standalone '-' symbol. Expected a literal. Did you mean 'sub'?")
    | ErrorSt -> failwith (err_prefix ^ "Invalid fragment '" ^ lexeme ^ "'.")

let st_int state =
    match state with
    | StartSt -> 0  | IdentSt -> 1  | IntLitSt -> 2   | FloatLitSt -> 3 | PercentSt -> 4 
    | LocalSt -> 5  | ColonSt -> 6  | PathSepSt -> 7  | PlusSt -> 8     | MinusSt -> 9 
    | ArrowSt -> 10 | EqualSt -> 11 | GreaterSt -> 12 | PunctSt -> 13   | ErrorSt -> 14

let ch_int char =
    match char with
    | Alpha -> 0 | Digit -> 1   | Percent -> 2 | Colon -> 3 | Plus -> 4   | Minus -> 5 
    | Equal -> 6 | Greater -> 7 | Period -> 8  | Punct -> 9 | Space -> 10 | Other -> 11

type char_pos = {
  mutable index  : int;
  mutable line   : int;
  mutable col    : int;
}

let next_token input pos = 
    let len = String.length input in
        (* update line and col counters if \n is found *)
        while pos.index < len && map_char input.[pos.index] = Space do
            if input.[pos.index] = '\n' then (
                pos.line <- pos.line + 1;
                pos.col <- 1
            ) else (
                pos.col <- pos.col + 1
            );
            pos.index <- pos.index + 1
        done;

        if pos.index >= len then (EOF, pos.line, pos.col)
        else 
            let start_pos = pos.index in
                (* fix line and col at beginning of token *)
                let token_line = pos.line in
                let token_col = pos.col in

                let rec traverse_state current_state index = 
                    if index >= len then
                        (current_state, index)

                    else
                        let current_char = map_char input.[index] in
                        let next_state = transition_table.(st_int current_state).(ch_int current_char) in (* effectively table.(row).(col) after resolving to ints *)

                            if next_state = ErrorSt then (* cannot consume next char *)
                                if index = start_pos then  (* case where first char is illegal, so we wrap it in a token alone *)
                                    (ErrorSt, index + 1) 
                                else
                                    (current_state, index)

                            else (* valid transition so consume it! nyom *)
                                    traverse_state next_state (index + 1)

                in 
                    let (final_state, end_pos) = traverse_state StartSt start_pos in
                    for i = start_pos to end_pos - 1 do
                        if input.[i] = '\n' then (
                            pos.line <- pos.line + 1;
                            pos.col <- 1
                        ) else (
                            pos.col <- pos.col + 1
                        ) done;

                        let lexeme = String.sub input start_pos (end_pos - start_pos) in
                            pos.index <- end_pos;
                            (map_states final_state lexeme token_line token_col, token_line, token_col)

let tokenize source_code = 
    let pos = {index = 0; line = 1; col = 1} in (* mutable pointers to track position *)
        let rec collect_tokens acc = 
            (* unpack tuple *)
            let (new_token, token_line, token_col) = next_token source_code pos in
                match new_token with
                | EOF -> List.rev ((EOF, token_line, token_col) :: acc) (* reversing list so tokens represent the source code in the correct order *)
                | _ -> collect_tokens ((new_token, token_line, token_col) :: acc)

        in collect_tokens []