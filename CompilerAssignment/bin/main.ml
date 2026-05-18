type token = 
(* Structrue and Declaration *)
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
LBracket | RBracket | LCurly | RCurly | LAngle | RAngle | Colon | Semicolon | DoubleColon | Arrow | Equal | Comma | Percent |
EOF

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
    | 'a' .. 'z' | 'A' .. 'Z' -> Alpha 
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

let start_state = 0
let ident_state = 1
let percent_state = 2
let local_state = 3
let colon_state = 4
let minus_state = 5
let space_state = 6