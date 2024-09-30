open Printf

(* registers *)
type reg = 
| RAX
| RSP
| RBP
| R10
| R11

(* arguments for instructions *)
type arg =
| Const of int64
| Reg of reg
| RegOffset of reg * int
| Label of string

(* asm instructions *)
type instruction =
| IRet
| IMov of arg * arg
| IAdd of arg * arg
| ISub of arg * arg
| ICmp of arg * arg
| IXor of arg * arg
| IJe  of arg
| IJle of arg
| IJmp of arg
| ILabel of arg
| ICall of arg
| IPush of arg
| IPop of arg
(* TO BE COMPLETED *)

let pp_reg reg : string =
  match reg with
  | RAX -> "RAX"
  | RSP -> "RSP"
  | RBP -> "RBP"
  | R10 -> "R10"
  | R11 -> "R11"

let pp_arg arg : string =
  match arg with
  | Const n -> sprintf "%#Lx" n
  | Reg r -> pp_reg r
  | RegOffset (r, i) -> sprintf "[%s - %i]" (pp_reg r) (8 * i)
  | Label l -> l

let pp_instr instr : string =
  match instr with
  | IRet -> "  ret" 
  | IMov (a1, a2) -> sprintf "  mov %s, %s" (pp_arg a1) (pp_arg a2)
  | IAdd (a1, a2) -> sprintf "  add %s, %s" (pp_arg a1) (pp_arg a2)
  | ISub (a1, a2) -> sprintf "  sub %s, %s" (pp_arg a1) (pp_arg a2)
  | ICmp (a1, a2) -> sprintf "  cmp %s, %s" (pp_arg a1) (pp_arg a2)
  | IXor (a1, a2) -> sprintf "  xor %s, %s" (pp_arg a1) (pp_arg a2)
  | IJe a1  -> sprintf "  je %s" (pp_arg a1)
  | IJle a1 -> sprintf "  jle %s" (pp_arg a1)
  | IJmp a1 -> sprintf "  jmp %s" (pp_arg a1)
  | ILabel a1 -> sprintf "%s:" (pp_arg a1)
  | ICall a1 -> sprintf "  call %s" (pp_arg a1)
  | IPush a1 -> sprintf "  push %s" (pp_arg a1)
  | IPop a1 -> sprintf "  pop %s" (pp_arg a1)
  (* TO BE COMPLETED *)

let pp_instrs (instrs : instruction list) : string =
  List.fold_left (fun res i -> res ^ "\n" ^ (pp_instr i)) "" instrs
