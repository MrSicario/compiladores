open Printf

(* registers *)
type reg = 
| RAX
| RSP
| RBP
| RDI (* arg_1 *)
| RSI (* arg_2 *)
| RDX (* arg_3 *)
| RCX (* arg_4 *)
| R8  (* arg_5 *)
| R9  (* arg_6 *) (* arg_(i+6) -> RBP + 8*(i+2) *)
| R10 (* scratch *)
| R11 (* scratch *)
| R14
| R15 (* heap *)

(* arguments for instructions *)
type arg =
| Const of int64
| Reg of reg
| RegOffset of reg * int
| RegIndex of reg * reg
| Label of string
| Rel of arg

(* asm instructions *)
type instruction =
| IBreak (* Line Break *)
| IComment of string
| IRet
| IMov of arg * arg
| IMovSize of string * arg * arg
| IAdd of arg * arg
| ISub of arg * arg
| ICmp of arg * arg
| IAnd of arg * arg
| IXor of arg * arg
| IShl of arg * arg
| IShr of arg * arg
| ISal of arg * arg
| ISar of arg * arg
| ITest of arg * arg
| IJe of arg
| IJne of arg
| IJz of arg
| IJnz of arg
| IJge of arg
| IJle of arg
| IJl of arg
| IJmp of arg
| ILabel of arg
| ICall of arg
| IPush of arg
| IPop of arg
| ILea of arg * arg

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
  | R14 -> "R14"
  | R15 -> "R15"

let rec pp_arg arg : string =
  match arg with
  | Const n -> sprintf "%#Lx" n
  | Reg r -> pp_reg r
  | RegOffset (r, i) -> 
    if i < 0
      then sprintf "[%s - 8*%i]" (pp_reg r) (-i)
      else sprintf "[%s + 8*%i]" (pp_reg r) (i)
  | RegIndex (r, i) -> sprintf "[%s + %s * 8]" (pp_reg r) (pp_reg i)
  | Label l -> l
  | Rel arg -> sprintf "[rel %s]" (pp_arg arg)

let pp_instr instr : string =
  match instr with
  | IBreak -> "" (* Agregado para mejorar legibilidad del assembly *)
  | IComment s -> sprintf "; %s" s
  | IRet -> "  ret" 
  | IMov (a1, a2) -> sprintf "  mov %s, %s" (pp_arg a1) (pp_arg a2)
  | IMovSize (a1, a2, a3) -> sprintf "  mov %s %s, %s" a1 (pp_arg a2) (pp_arg a3)
  | IAdd (a1, a2) -> sprintf "  add %s, %s" (pp_arg a1) (pp_arg a2)
  | ISub (a1, a2) -> sprintf "  sub %s, %s" (pp_arg a1) (pp_arg a2)
  | ICmp (a1, a2) -> sprintf "  cmp %s, %s" (pp_arg a1) (pp_arg a2)
  | IAnd (a1, a2) -> sprintf "  and %s, %s" (pp_arg a1) (pp_arg a2)
  | IXor (a1, a2) -> sprintf "  xor %s, %s" (pp_arg a1) (pp_arg a2)
  | IShl (a1, a2) -> sprintf "  shl %s, %s" (pp_arg a1) (pp_arg a2)
  | IShr (a1 ,a2) -> sprintf "  shr %s, %s" (pp_arg a1) (pp_arg a2)
  | ISal (a1, a2) -> sprintf "  sal %s, %s" (pp_arg a1) (pp_arg a2)
  | ISar (a1, a2) -> sprintf "  sar %s, %s" (pp_arg a1) (pp_arg a2)
  | ITest (a1, a2) -> sprintf "  test %s, %s" (pp_arg a1) (pp_arg a2)
  | IJe a1  -> sprintf "  je %s" (pp_arg a1)
  | IJne a1 -> sprintf "  jne %s" (pp_arg a1)
  | IJz a1 -> sprintf "  jz %s" (pp_arg a1)
  | IJnz a1 -> sprintf "  jnz %s" (pp_arg a1)
  | IJge a1 -> sprintf "  jge %s" (pp_arg a1)
  | IJle a1 -> sprintf "  jle %s" (pp_arg a1)
  | IJl a1 -> sprintf "  jl %s" (pp_arg a1)
  | IJmp a1 -> sprintf "  jmp %s" (pp_arg a1)
  | ILabel a1 -> sprintf "%s:" (pp_arg a1)
  | ICall a1 -> sprintf "  call %s" (pp_arg a1)
  | IPush a1 -> sprintf "  push %s" (pp_arg a1)
  | IPop a1 -> sprintf "  pop %s" (pp_arg a1)
  | ILea (a1, a2) -> sprintf "  lea %s, %s" (pp_arg a1) (pp_arg a2)
  (* TO BE COMPLETED *)

let pp_instrs (instrs : instruction list) : string =
  List.fold_left (fun res i -> res ^ "\n" ^ (pp_instr i)) "" instrs
