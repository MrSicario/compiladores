open Ast
open Asm
open Anf
open Gensym
open Parse
open Checks

(* Compile constants *)
let bool_true = 0x8000000000000001L (* 0b10...01 *)
let bool_false = 1L (* 0b0...1 *)
let min_int = Int64.div Int64.min_int 2L
let max_int = Int64.div Int64.max_int 2L

let pointer_mask = 0x7L
let tuple_tag = 0x3L
let closure_tag = 0x5L

(* Lexical Environment *)
type env = (string * (reg * int)) list
let empty_env = []
let extend_env (name : string) (reg : reg) (env : env) : (env * int) =
  let slot = 1 + (List.length env) in
  ((name, (reg, -slot))::env, -slot)

let lookup_env (name : string) (l_env : env) (e_env : env) : reg * int =
  let rec lookup_env (name : string) (env : env) : reg * int =
    match env with
    | [] -> raise (CTError (Printf.sprintf "Free identifier: %s" name))
    | (n, (reg, i))::tail ->
      if n = name then (reg, i) else (lookup_env name tail)
  in if List.mem_assoc name l_env then lookup_env name l_env
  else if List.mem_assoc name e_env then lookup_env name e_env
  else raise (CTError (Printf.sprintf "Free identifier: %s" name))

(* Function Environment *)
type afenv = afundef list
let empty_afenv : afenv = []
let rec lookup_afenv : string -> afenv -> afundef =
  fun s fenv -> 
    match fenv with
    | [] -> raise (CTError (Printf.sprintf "Undefined function: %s" s))
    | (f::fs) -> if afundef_name f = s then f else lookup_afenv s fs

let get_external_funcs (fenv:afenv) =
  let rec get_external_funcs fenv r =
    match fenv with
    | (DefSys(name,_,_)::fs) -> (get_external_funcs fs (r @ [name]))
    | (DefFun(_,_,_)::fs) -> (get_external_funcs fs r)
    | [] -> r
  in (get_external_funcs fenv [])

(* Compiler *)

(* Untag/tag the value currently in RAX as the specified type **)
let untag_type_at reg ctype =
  match ctype with
  | CInt -> [ ISar (Reg reg, Const 1L) ]
  | CBool -> [ IShr (Reg reg, Const 63L) ]
  | CAny -> []
  | CTuple _ -> [ ISub (Reg reg, Const tuple_tag) ]

let tag_type_at reg ctype =
  match ctype with
  | CInt -> [ ISal (Reg reg, Const 1L) ]
  | CBool -> [ IShl (Reg reg, Const 63L) ] @ [ IAdd (Reg reg, Const 1L) ]
  | CAny -> []
  | CTuple _ -> [ IAdd (Reg reg, Const tuple_tag) ]

let untag_type ctype = untag_type_at RAX ctype
let tag_type ctype = tag_type_at RAX ctype

let test_if_number =
  [ ITest (Reg RAX, Const 1L) ]
  @ [ IJnz (Label "error_not_number") ]

let test_if_bool =
  [ ITest (Reg RAX, Const 1L) ]
  @ [ IJz (Label "error_not_bool") ]

(* destructive*)
let test_if_tuple =
  [ IAnd (Reg RAX, Const (pointer_mask)) ]
  @ [ ICmp (Reg RAX, Const (tuple_tag)) ]
  @ [ IJne  (Label "error_not_tuple") ]

let test_type ctype =
    match ctype with
    | CInt -> test_if_number
    | CBool -> test_if_bool
    | CAny -> []
    | CTuple _ -> test_if_tuple

let error_handlers =
  let error_not_number =
    [ ILabel (Label "error_not_number") ]
    @ [ IMov (Reg RDI, Const 1L) ]
    @ [ IMov (Reg RSI, Reg RAX) ]
    @ [ ICall (Label "error") ]
  in
  let error_not_bool =
    [ ILabel (Label "error_not_bool") ]
    @ [ IMov (Reg RDI, Const 2L) ]
    @ [ IMov (Reg RSI, Reg RAX) ]
    @ [ ICall (Label "error") ]
  in
  let error_not_tuple =
    [ ILabel (Label "error_not_tuple") ]
    @ [ IMov (Reg RDI, Const 3L) ]
    @ [ IMov (Reg RSI, Reg RAX) ]
    @ [ ICall (Label "error")]
  in
  let error_index =
    [ ILabel (Label "error_index") ]
    @ [ IMov (Reg RDI, Const 10L) ]
    @ tag_type (CTuple [])
    @ tag_type_at R10 CInt
    @ [ IMov (Reg RSI, Reg RAX) ]
    @ [ IMov (Reg RDX, Reg R10) ]
    @ [ ICall (Label "error") ]
  in String.concat "\n" (List.map pp_instrs [error_not_number ; error_not_bool ; error_not_tuple ; error_index])

let rec compile_aexpr (expr : aexpr) (l_env : env) (e_env : env) (fenv : afenv) : instruction list =
  match expr with
  | Let (id, c, a) ->
    let (l_env', slot) = extend_env id RBP l_env in
    (compile_cexpr c l_env e_env fenv)
    @ [ IMov (RegOffset (RBP, slot), Reg (RAX)) ]
    @ (compile_aexpr a l_env' e_env fenv)
  | If (cond_expr, then_expr, else_expr) ->
    let else_label = Gensym.fresh "else" in
    let done_label = Gensym.fresh "done" in
    [ IMov (Reg RAX, arg_immexpr cond_expr l_env e_env) ]
    @ test_if_bool
    @ [ ICmp (Reg RAX, Const (bool_false)) ]
    @ [ IJe  (Label else_label) ]
    @ (compile_aexpr then_expr l_env e_env fenv)
    @ [ IJmp (Label done_label) ]
    @ [ ILabel (Label else_label) ]
    @ (compile_aexpr else_expr l_env e_env fenv)
    @ [ ILabel (Label done_label)]
  | Ret c -> compile_cexpr c l_env e_env fenv

and compile_cexpr (expr : cexpr) (l_env : env) (e_env : env) (fenv : afenv) : instruction list =
  match expr with
  | Atom i -> [ IMov (Reg RAX, arg_immexpr i l_env e_env) ]
  | Prim1 (op, c) -> 
    let head = compile_cexpr c l_env e_env fenv in
    begin match op with
    | Add1 -> head @ test_if_number @ [ IAdd (Reg(RAX), arg_immexpr (Num 1L) empty_env e_env) ]
    | Sub1 -> head @ test_if_number @ [ ISub (Reg(RAX), arg_immexpr (Num 1L) empty_env e_env) ]
    | Not -> 
      head 
      @ test_if_bool
      @ [ IMov (Reg(R10), Const(Int64.sub bool_true 1L)) ] 
      @ [ IXor (Reg(RAX), Reg(R10)) ]
    | Print ->
      head
      @ [ IPush (Reg R9) ]
      @ [ IPush (Reg R8) ]
      @ [ IPush (Reg RCX) ]
      @ [ IPush (Reg RDX) ]
      @ [ IPush (Reg RSI) ]
      @ [ IPush (Reg RDI) ]
      @ [ IMov (Reg RDI, Reg RAX)]
      @ [ ICall (Label "print")]
      @ [ IPop (Reg RDI) ]
      @ [ IPop (Reg RSI) ]
      @ [ IPop (Reg RDX) ]
      @ [ IPop (Reg RCX) ]
      @ [ IPop (Reg R8) ]
      @ [ IPop (Reg R9) ]
    end
  | Prim2 (op, imm1, imm2) -> 
    let const1 = [ IMov (Reg RAX, arg_immexpr imm1 l_env e_env) ] in
    let const2 = [ IMov (Reg RAX, arg_immexpr imm2 l_env e_env) ] in
    begin match op with
    | Add -> 
      const1
      @ test_if_number
      @ [ IMov (Reg R10, Reg RAX) ]
      @ const2
      @ test_if_number
      @ [ IMov (Reg RAX, Reg R10) ]
      @ [ IAdd (Reg RAX, arg_immexpr imm2 l_env e_env) ]
    | And -> 
      const1
      @ test_if_bool
      @ (and_immexpr imm2 l_env e_env)
    | Or -> 
      const1
      @ test_if_bool 
      @ (or_immexpr imm2 l_env e_env)
    | Lte -> 
      const1
      @ test_if_number
      @ [ IMov (Reg R10, Reg RAX) ]
      @ const2
      @ test_if_number
      @ [ IMov (Reg RAX, Reg R10) ]
      @ (lte_immexpr imm2 l_env e_env)
    | Get ->
      const2 (* index *)
      @ test_if_number
      @ untag_type CInt
      @ [ IMov (Reg R10, Reg RAX) ]
      @ const1 (* tuple *)
      @ test_if_tuple
      @ const1
      @ untag_type (CTuple [])
      @ [ IMov (Reg R11, RegOffset(RAX, 0)) ] (* get tuple size and check bounds*)
      @ [ ICmp (Reg R10, Reg R11)]
      @ [ IJge (Label "error_index") ]
      @ [ ICmp (Reg R10, Const 0L) ]
      @ [ IJl  (Label "error_index") ]
      @ [ IAdd (Reg R10, Const 1L) ] (* finally get the value at idx+1 to skip over the size slot *)
      @ [ IMov (Reg RAX, RegIndex (RAX, R10))] (* get the value *)
    end
  | Apply (name, args) ->
    let caller_saved_push =
      [ IPush (Reg R9) ]
      @ [ IPush (Reg R8) ]
      @ [ IPush (Reg RCX) ]
      @ [ IPush (Reg RDX) ]
      @ [ IPush (Reg RSI) ]
      @ [ IPush (Reg RDI) ]
    in let caller_saved_pop =
      [ IPop (Reg RDI) ]
      @ [ IPop (Reg RSI) ]
      @ [ IPop (Reg RDX) ]
      @ [ IPop (Reg RCX) ]
      @ [ IPop (Reg R8) ]
      @ [ IPop (Reg R9) ]
    in begin match lookup_afenv name fenv with
    | DefFun (name, _, _) ->
      let arg_instrs = build_args args l_env e_env
      in let call_instr = 
        let n = List.length args in
        if n <= 6 
          then [ ICall (Label name) ]
          else let x = Int64.of_int (8 * (n-6)) in
          [ ICall (Label name) ] @ [ IAdd (Reg RSP, Const x) ]
      in caller_saved_push @ arg_instrs @ call_instr @ caller_saved_pop
    | DefSys (name, arg_typelist, ret_type) ->
      let arg_instrs = build_test_args arg_typelist args l_env e_env
      in let call_instr = 
        let n = List.length args in
        if n <= 6
          then [ ICall (Label name) ]
          else let x = Int64.of_int (8 * (n-6)) in
          [ ICall (Label name) ] @ [ IAdd (Reg RSP, Const x) ]
      in caller_saved_push @ arg_instrs @ call_instr @ caller_saved_pop @ tag_type ret_type @ test_type ret_type
    end
  | LamApply (_ (*lambda*), _ (*args*)) -> failwith "TBD"
  | Tuple imm_list ->
    let n = List.length imm_list
    in let rec move_elems elems n =
      match elems with
      | [] -> []
      | hd::tl ->
        [ IComment ("store elem in slot " ^ (Int.to_string n))]
        @ [ IMov (Reg RAX, arg_immexpr hd l_env e_env) ]
        @ [ IMovSize ("qword", RegOffset (R15, n), Reg RAX) ]
        @ move_elems tl (n+1)
    in
    [ IComment "store the size of the tuple in slot 0"]
    @ [ IMovSize ("qword", RegOffset (R15, 0), Const (Int64.of_int n)) ]
    @ move_elems imm_list 1
    @ [ IMov (Reg RAX, Reg R15) ]
    @ tag_type (CTuple [])
    @ [ IAdd (Reg R15, Const(Int64.of_int (8 * (n+1)))) ] (* bump heap pointer *)
  | Set (t, k, v) ->
    [ IMov (Reg RAX, arg_immexpr k l_env e_env) ] (* index *)
    @ test_if_number
    @ untag_type CInt
    @ [ IMov (Reg R10, Reg RAX) ]
    @ [ IMov (Reg RAX, arg_immexpr t l_env e_env) ] (* tuple *)
    @ test_if_tuple
    @ [ IMov (Reg RAX, arg_immexpr t l_env e_env) ] (* restore tuple *)
    @ untag_type (CTuple [])
    @ [ IMov (Reg R11, RegOffset(RAX, 0)) ] (* get tuple size on R11 and check bounds*)
    @ [ ICmp (Reg R10, Reg R11)]
    @ [ IJge (Label "error_index") ]
    @ [ ICmp (Reg R10, Const 0L) ]
    @ [ IJl  (Label "error_index") ]
    @ [ IAdd (Reg R10, Const 1L) ] (* finally set the value at idx+1 to skip over the size slot *)
    @ [ IMov (Reg R11, arg_immexpr v l_env e_env) ]
    @ [ IMovSize ("qword", RegIndex (RAX, R10), Reg R11)] (* set the value *)
    @ tag_type (CTuple []) (* return the same tuple pointer *)

and build_args args l_env e_env =
  let rec inner_rec args instrs =
    match args with
    | [] -> instrs
    | hd::tl ->
      begin match List.length instrs with
      | 0 -> inner_rec tl ([ IMov (Reg RDI, arg_immexpr hd l_env e_env) ] @ instrs)
      | 1 -> inner_rec tl ([ IMov (Reg RSI, arg_immexpr hd l_env e_env) ] @ instrs)
      | 2 -> inner_rec tl ([ IMov (Reg RDX, arg_immexpr hd l_env e_env) ] @ instrs)
      | 3 -> inner_rec tl ([ IMov (Reg RCX, arg_immexpr hd l_env e_env) ] @ instrs)
      | 4 -> inner_rec tl ([ IMov (Reg R8, arg_immexpr hd l_env e_env) ] @ instrs)
      | 5 -> inner_rec tl ([ IMov (Reg R9, arg_immexpr hd l_env e_env) ] @ instrs)
      | n when n > 5 -> inner_rec tl ([ IPush (arg_immexpr hd l_env e_env) ] @ instrs)
      | _ -> failwith "Negative list length"
      end
  in inner_rec args []

(* Like build_args, but type checks each argument first *)
and build_test_args expected_types args l_env e_env =
  let rec inner_rec expected_types args instrs acount =
    match args with
    | [] -> instrs
    | hd::tl ->
      let expected_type = List.hd expected_types
      in let move_test_untag =
        [ IMov (Reg RAX, arg_immexpr hd l_env e_env)]
        @ test_type expected_type
        @ untag_type expected_type
      in begin match acount with
      | 0 -> inner_rec (List.tl expected_types) tl ( move_test_untag @ [ IMov (Reg RDI, Reg RAX) ] @ instrs) (acount+1)
      | 1 -> inner_rec (List.tl expected_types) tl ( move_test_untag @ [ IMov (Reg RSI, Reg RAX) ] @ instrs) (acount+1)
      | 2 -> inner_rec (List.tl expected_types) tl ( move_test_untag @ [ IMov (Reg RDX, Reg RAX) ] @ instrs) (acount+1)
      | 3 -> inner_rec (List.tl expected_types) tl ( move_test_untag @ [ IMov (Reg RCX, Reg RAX) ] @ instrs) (acount+1)
      | 4 -> inner_rec (List.tl expected_types) tl ( move_test_untag @ [ IMov (Reg R8,  Reg RAX) ] @ instrs) (acount+1)
      | 5 -> inner_rec (List.tl expected_types) tl ( move_test_untag @ [ IMov (Reg R9,  Reg RAX) ] @ instrs) (acount+1)
      | n when n > 5 -> inner_rec (List.tl expected_types) tl ( move_test_untag @ [ IPush (Reg RAX) ] @ instrs) (acount+1)
      | _ -> failwith "Negative argument count"
      end
  in inner_rec expected_types args [] 0

and arg_immexpr (expr : immexpr) (l_env : env) (e_env : env) : arg =
  match expr with
  | Num n -> 
    if n > max_int || n < min_int 
      then raise (CTError (Printf.sprintf "Integer overflow: " ^ (Int64.to_string n)))
      else Const (Int64.shift_left n 1)
  | Bool true -> Const (bool_true)
  | Bool false -> Const (bool_false)
  | Id x -> 
    let reg, n = lookup_env x l_env e_env in
    if n = 0 then Reg reg else RegOffset (reg, n)
  | Lambda (_(*params*), _(* expr *)) -> failwith "TBD"

and and_immexpr (expr : immexpr) (l_env : env) (e_env : env) : instruction list =
  let done_label = Gensym.fresh "and" in
  [ IMov (Reg R10, Const bool_false) ]
  @ [ ICmp (Reg RAX, Reg R10) ]
  @ [ IJe (Label done_label) ] (* if arg0 is false -> false *)
  @ [ IMov (Reg RAX, arg_immexpr expr l_env e_env) ]
  @ test_if_bool
  @ [ ICmp (Reg RAX, Reg R10) ]
  @ [ IJe (Label done_label) ] (* if arg1 is false -> false*)
  @ [ IMov (Reg RAX, Const bool_true) ] (* else -> true *)
  @ [ ILabel (Label done_label) ]

and or_immexpr (expr : immexpr) (l_env : env) (e_env : env) : instruction list =
  let done_label = Gensym.fresh "or" in
  [ IMov (Reg R10, Const bool_true) ]
  @ [ ICmp (Reg RAX, Reg R10) ]
  @ [ IJe (Label done_label) ]
  @ [ IMov (Reg R10, Reg RAX) ]
  @ [ IMov (Reg RAX, arg_immexpr expr l_env e_env) ]
  @ test_if_bool
  @ [ ICmp (Reg RAX, Reg R10) ]
  @ [ IJe (Label done_label) ]
  @ [ IMov (Reg RAX, Const bool_true) ]
  @ [ ILabel (Label done_label) ]

and lte_immexpr (expr : immexpr) (l_env : env) (e_env : env) : instruction list =
  let lte_label = Gensym.fresh "lte" in
  let done_label = Gensym.fresh "ltedone" in
  [ IMov (Reg R10, arg_immexpr expr l_env e_env)]
  @ [ ICmp (Reg RAX, Reg R10) ]
  @ [ IJle (Label lte_label) ]
  @ [ IMov (Reg RAX, Const bool_false) ]
  @ [ IJmp (Label done_label)]
  @ [ ILabel (Label lte_label) ]
  @ [ IMov (Reg RAX, Const bool_true) ]
  @ [ ILabel (Label done_label) ]

let compile_afundefs (fenv: afundef list) : instruction list =
  let rec gen_64bits_env l env =
    match l with
    | [] -> env
    | hd::tl ->
      begin match List.length env with
      | 0 -> 
        let env' = (hd, (RDI, 0))::env in
        gen_64bits_env tl env'
      | 1 -> 
        let env' = (hd, (RSI, 0))::env in
        gen_64bits_env tl env'
      | 2 -> 
        let env' = (hd, (RDX, 0))::env in
        gen_64bits_env tl env'
      | 3 ->
        let env' = (hd, (RCX, 0))::env in
        gen_64bits_env tl env'
      | 4 ->
        let env' = (hd, (R8, 0))::env in
        gen_64bits_env tl env'
      | 5 ->
        let env' = (hd, (R9, 0))::env in
        gen_64bits_env tl env'
      | n when n >= 6 ->
        let env' = (hd, (RBP, (n-4)))::env in
        gen_64bits_env tl env'
      | _ -> failwith "Failed pattern matching"
      end
  in
  let compile_afundef f fenv =
    match f with
    | DefFun (name, args, expr) ->
      [ ILabel (Label name) ]
      @ [ IPush (Reg RBP) ]
      @ [ IMov (Reg RBP, Reg RSP) ]
      @ [ ISub (Reg RSP, Const (Int64.of_int (8 * (get_depth expr)))) ]
      @ compile_aexpr expr empty_env (gen_64bits_env args []) fenv
      @ [ IMov (Reg RSP, Reg RBP) ]
      @ [ IPop (Reg RBP) ]
      @ [ IRet ]
    | DefSys (_, _, _) -> []
  in let rec accumulate fl acc =
    match fl with
    | [] -> acc
    | hd :: tail -> 
      let instrs = compile_afundef hd fenv in
      accumulate tail (acc @ [ IBreak ] @ instrs)
  in accumulate fenv []

let gen_prologue afenv =
  let externs = (List.map (String.cat "\nextern ") (get_external_funcs afenv)) in
  let header = "
section .text
extern error
extern print" ^ (String.concat "" externs) ^ "
global our_code_starts_here" in header

let compile_prog (p : prog) : string =
  let f, e = p in
  let afenv = check_afundefs (List.map anf_fundef f) in
  let aexpr = check_anf (anf_expr e) afenv in
  let instrs = compile_aexpr aexpr empty_env empty_env afenv in
  let functions = pp_instrs (compile_afundefs afenv) ^ "\n" in
  let prologue = gen_prologue afenv in
  let body = "
our_code_starts_here:
  push RBP
  mov RBP, RSP
  mov R15, RDI
  add R15, 7
  mov R11, 0xfffffffffffffff8
  and R15, R11" ^ (let depth = get_depth aexpr in
  if depth = 0
    then ""
    else "
  sub RSP, 8*" ^ string_of_int (depth + depth mod 2)) ^ pp_instrs instrs in
  let epilogue ="
  mov RSP, RBP
  pop RBP
  ret" in
  prologue ^ functions ^ body ^ epilogue ^ "\n" ^ error_handlers
