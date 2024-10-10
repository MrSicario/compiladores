open Printf

(* registers *)
type reg = 
| RAX
| RSP
| RBP
| R10
| R11
| RDI (* arg_1 *)
| RSI (* arg_2 *)
| RDX (* arg_3 *)
| RCX (* arg_4 *)
| R8  (* arg_5 *)
| R9  (* arg_6 *)
      (* arg_(i+6) -> RBP + 8*(i+2) *)

(* arguments for instructions *)
type arg =
| Const of int64
| Reg of reg
| RegOffset of reg * int
| Label of string

(* asm instructions *)
type instruction =
| IBreak (* Line Break *)
| IRet
| IMov of arg * arg
| IAdd of arg * arg
| ISub of arg * arg
| ICmp of arg * arg
| IXor of arg * arg
| IShl of arg * arg
| IShr of arg * arg
| ISal of arg * arg
| ISar of arg * arg
| ITest of arg * arg
| IJe  of arg
| IJz of arg
| IJnz of arg
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
  | RSI -> "RSI"
  | RDI -> "RDI"
  | RDX -> "RDX"
  | RCX -> "RCX"
  | R8  -> "R8"
  | R9  -> "R9"
  | R10 -> "R10"
  | R11 -> "R11"

let pp_arg arg : string =
  match arg with
  | Const n -> sprintf "%#Lx" n
  | Reg r -> pp_reg r
  | RegOffset (r, i) -> 
    if i < 0
      then sprintf "[%s - 8*%i]" (pp_reg r) (-i)
      else sprintf "[%s + 8*%i]" (pp_reg r) (i)
  | Label l -> l

let pp_instr instr : string =
  match instr with
  | IBreak -> "" (* Agregado para mejorar legibilidad del assembly *)
  | IRet -> "  ret" 
  | IMov (a1, a2) -> sprintf "  mov %s, %s" (pp_arg a1) (pp_arg a2)
  | IAdd (a1, a2) -> sprintf "  add %s, %s" (pp_arg a1) (pp_arg a2)
  | ISub (a1, a2) -> sprintf "  sub %s, %s" (pp_arg a1) (pp_arg a2)
  | ICmp (a1, a2) -> sprintf "  cmp %s, %s" (pp_arg a1) (pp_arg a2)
  | IXor (a1, a2) -> sprintf "  xor %s, %s" (pp_arg a1) (pp_arg a2)
  | IShl (a1, a2) -> sprintf "  shl %s, %s" (pp_arg a1) (pp_arg a2)
  | IShr (a1 ,a2) -> sprintf "  shr %s, %s" (pp_arg a1) (pp_arg a2)
  | ISal (a1, a2) -> sprintf "  sal %s, %s" (pp_arg a1) (pp_arg a2)
  | ISar (a1, a2) -> sprintf "  sar %s, %s" (pp_arg a1) (pp_arg a2)
  | ITest (a1, a2) -> sprintf "  test %s, %s" (pp_arg a1) (pp_arg a2)
  | IJe a1  -> sprintf "  je %s" (pp_arg a1)
  | IJz a1 -> sprintf "  jz %s" (pp_arg a1)
  | IJnz a1 -> sprintf "  jnz %s" (pp_arg a1)
  | IJle a1 -> sprintf "  jle %s" (pp_arg a1)
  | IJmp a1 -> sprintf "  jmp %s" (pp_arg a1)
  | ILabel a1 -> sprintf "%s:" (pp_arg a1)
  | ICall a1 -> sprintf "  call %s" (pp_arg a1)
  | IPush a1 -> sprintf "  push %s" (pp_arg a1)
  | IPop a1 -> sprintf "  pop %s" (pp_arg a1)
  (* TO BE COMPLETED *)

let pp_instrs (instrs : instruction list) : string =
  List.fold_left (fun res i -> res ^ "\n" ^ (pp_instr i)) "" instrs
