open Types

let map_keywords s = 
    match s with
    | "extern" -> Extern | "type" -> Type | "function" -> Function | "locals" -> Locals | "void" -> Void | "entry" -> Entry | "to" -> To 
    | "const" -> Const | "cast" -> Cast | "un" -> Un | "bin" -> Bin | "addr_of" -> Addr_Of | "member_ptr" -> Member_Ptr | "load" -> Load | "store" -> Store | "call" -> Call 
        | "jump" -> Jump | "cjump" -> Cjump | "return" -> Return 
    | "bool" -> Bool | "i32" -> I32 | "i64" -> I64 | "u32" -> U32 | "f64" -> F64 | "ptr" -> Ptr
    | "true" -> True | "false" -> False | "null" -> Null 
    | _ -> Ident s 

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

        if pos.index >= len then EOF
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
                        let lexeme = String.sub input start_pos (end_pos - start_pos) in
                            pos.index <- end_pos;
                            map_states final_state lexeme token_line token_col

let tokenize source_code = 
    let pos = {index = 0; line = 1; col = 1} in (* mutable pointers to track position *)
        let rec collect_tokens acc = 
            let new_token = next_token source_code pos in
                match new_token with
                | EOF -> List.rev ((EOF, pos.line, pos.col) :: acc) (* reversing list so tokens represent the source code in the correct order *)
                | _ -> collect_tokens ((new_token, pos.line, pos.col) :: acc)

        in collect_tokens []