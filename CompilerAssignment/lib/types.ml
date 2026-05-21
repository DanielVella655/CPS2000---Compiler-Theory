(* Types Required in Lexer *)

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

type chars = 
| Alpha | Digit | Percent | Colon | Plus | Minus | Equal | Greater | Period | Punct | Space | Other

type states = 
| StartSt | IdentSt | IntLitSt | FloatLitSt | PercentSt | LocalSt | ColonSt | PathSepSt | PlusSt | MinusSt 
| ArrowSt | EqualSt | GreaterSt | PunctSt | ErrorSt

type char_pos = {
  mutable index  : int;
  mutable line   : int;
  mutable col    : int;
}


(* Types required in Parser*)

type path = string list   (* ["Custom"; "Struct"; "Point"]*)
type local = string       (* "%var"*)
type label = string       (* "bb0" *)

type prim_type = | TBool | TI32 | TI64 | TU32 | TF64

type resir_type = | Prim of prim_type | Path of path | TPtr of resir_type (* ptr<Type> *)

type literal = TIntLit of int | TFloatLit of float | 