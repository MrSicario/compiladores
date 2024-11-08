open Ast
open Asm
open Anf
open Gensym
open Parse
open Checks
open Printf

(* Compile constants *)
let bool_true = 0x8000000000000001L (* 0b1...001 *)
let bool_false = 0x1L (* 0b01...1 *)
let min_int = Int64.div Int64.min_int 2L
let max_int = Int64.div Int64.max_int 2L
let bool_tag = 0b001L
let tuple_tag = 0b011L
let closure_tag = 0b101L
let pointer_mask = 0b111L
(* Lexical Environment *)
type env = Env of int * int * (string * (reg * int)) list (* |RBP vars| * |R15 vars| * vars *)
let empty_env = Env (0, 0, [])
let extend_env (name : string) (reg : reg) ?(fun_param:int = 0) (env : env) : (env * int) =
  let Env (rbp, r15, lst) = env in
  match reg with
  | RBP -> 
    if fun_param = 0
      then let slot = rbp+1 in
        ((Env (slot, r15, (name, (RBP, -slot))::lst)), -slot)
      else
        ((Env (rbp, r15, (name, (RBP, fun_param))::lst)), fun_param)
  | R15 ->
    let slot = r15+1 in
    ((Env (rbp, slot, (name, (R15, -slot))::lst)), -slot)
  | _ -> ((Env (rbp, r15, (name, (reg, 0))::lst)), 0)

let lookup_env (name : string) (env : env) : reg * int =
  let Env(_, _, lst) = env in
  if List.mem_assoc name lst then List.assoc name lst
  else raise (CTError (sprintf "Free identifier: %s" name))

(* Function Environment *)
type afenv = afundef list
let empty_afenv : afenv = []
let rec lookup_afenv : string -> afenv -> afundef =
  fun s fenv -> 
    match fenv with
    | [] -> raise (CTError (sprintf "Undefined function: %s" s))
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
  [ IMov (Reg R11, Reg RAX) ]
  @ [ IAnd (Reg R11, Const pointer_mask) ]
  @ [ ICmp (Reg R11, Const bool_tag) ]
  @ [ IJne (Label "error_not_bool") ]

(* destructive*)
let test_if_tuple =
  [ IAnd (Reg RAX, Const (pointer_mask)) ]
  @ [ ICmp (Reg RAX, Const (tuple_tag)) ]
  @ [ IJne  (Label "error_not_tuple") ]

let test_if_closure =
  [ IMov (Reg R11, Reg RAX) ]
  @ [ IAnd (Reg R11, Const (pointer_mask)) ]
  @ [ ICmp (Reg R11, Const closure_tag) ]
  @ [ IJne (Label "error_not_closure") ]

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
  let error_not_closure =
    [ ILabel (Label "error_not_closure") ]
    @ [ IMov (Reg RDI, Const 4L) ]
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
  in
  let error_arity_mismatch =
    [ ILabel (Label "error_wrong_arity") ]
    @ [ IMov (Reg RDI, Const 5L) ]
    @ [ IMov (Reg R10, Qword (RegOffset (RAX, 0))) ]
    @ [ ISal (Reg R10, Const 1L) ]
    @ [ ISal (Reg R11, Const 1L) ]
    @ [ IMov (Reg RSI, Reg R10) ]
    @ [ IMov (Reg RDX, Reg R11) ]
    @ [ ICall (Label "error") ]
  in String.concat "\n" (List.map pp_instrs 
  [error_not_number ; error_not_bool ; error_not_tuple ; error_not_closure ; error_index; error_arity_mismatch])

let rec compile_aexpr (expr : aexpr) (env : env) (fenv : afenv) : instruction list =
  match expr with
  | Let (id, c, a) ->
    let (env', slot) = extend_env id RBP env in
    (compile_cexpr c env fenv)
    @ [ IMov (RegOffset (RBP, slot), Reg (RAX)) ]
    @ (compile_aexpr a env' fenv)
  | If (cond_expr, then_expr, else_expr) ->
    let else_label = Gensym.fresh "else" in
    let done_label = Gensym.fresh "done" in
    compile_immexpr cond_expr env fenv
    @ test_if_bool
    @ [ ICmp (Reg RAX, Const (bool_false)) ]
    @ [ IJe  (Label else_label) ]
    @ (compile_aexpr then_expr env fenv)
    @ [ IJmp (Label done_label) ]
    @ [ ILabel (Label else_label) ]
    @ (compile_aexpr else_expr env fenv)
    @ [ ILabel (Label done_label)]
  | Ret c -> compile_cexpr c env fenv

and compile_cexpr (expr : cexpr) (env : env) (fenv : afenv) : instruction list =
  match expr with
  | Atom i -> compile_immexpr i env fenv
  | Prim1 (op, c) -> 
    let head = compile_cexpr c env fenv in
    begin match op with
    | Add1 -> head @ test_if_number @ [ IAdd (Reg(RAX), Const 0b10L) ]
    | Sub1 -> head @ test_if_number @ [ ISub (Reg(RAX), Const 0b10L) ]
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
    let instr1 = compile_immexpr imm1 env fenv in
    let instr2 = compile_immexpr imm2 env fenv in
    begin match op with
    | Add -> 
      instr2
      @ test_if_number
      @ [ IMov (Reg R10, Reg RAX) ]
      @ instr1
      @ test_if_number
      @ [ IAdd (Reg RAX, Reg R10) ]
    | And -> 
      let done_label = Gensym.fresh "and" in
      instr1
      @ test_if_bool
      @ [ IMov (Reg R10, Const bool_false) ]
      @ [ ICmp (Reg RAX, Reg R10) ]
      @ [ IJe (Label done_label) ] (* if arg0 is false -> false *)
      @ instr2
      @ test_if_bool
      @ [ ICmp (Reg RAX, Reg R10) ]
      @ [ IJe (Label done_label) ] (* if arg1 is false -> false*)
      @ [ IMov (Reg RAX, Const bool_true) ] (* else -> true *)
      @ [ ILabel (Label done_label) ]
    | Or -> 
      let done_label = Gensym.fresh "or" in
      instr1
      @ test_if_bool 
      @ [ IMov (Reg R10, Const bool_true) ]
      @ [ ICmp (Reg RAX, Reg R10) ]
      @ [ IJe (Label done_label) ]
      @ [ IMov (Reg R10, Reg RAX) ]
      @ instr2
      @ test_if_bool
      @ [ ICmp (Reg RAX, Reg R10) ]
      @ [ IJe (Label done_label) ]
      @ [ IMov (Reg RAX, Const bool_true) ]
      @ [ ILabel (Label done_label) ]
    | Lte -> 
      let lte_label = Gensym.fresh "lte" in
      let done_label = Gensym.fresh "ltedone" in
      instr1
      @ test_if_number
      @ [ IMov (Reg R10, Reg RAX) ]
      @ instr2
      @ test_if_number
      @ [ IMov (Reg RAX, Reg R10) ]
      @ compile_immexpr imm2 ~dst:R10 env fenv
      @ [ ICmp (Reg RAX, Reg R10) ]
      @ [ IJle (Label lte_label) ]
      @ [ IMov (Reg RAX, Const bool_false) ]
      @ [ IJmp (Label done_label)]
      @ [ ILabel (Label lte_label) ]
      @ [ IMov (Reg RAX, Const bool_true) ]
      @ [ ILabel (Label done_label) ]
    | Get ->
      instr2 (* index *)
      @ test_if_number
      @ untag_type CInt
      @ [ IMov (Reg R10, Reg RAX) ]
      @ instr1 (* tuple *)
      @ test_if_tuple
      @ instr1
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
      let arg_instrs = build_args args env fenv
      in let call_instr = 
        let n = List.length args in
        if n <= 6 
          then [ ICall (Label name) ]
          else let x = Int64.of_int (8 * (n-6)) in
          [ ICall (Label name) ] @ [ IAdd (Reg RSP, Const x) ]
      in caller_saved_push @ arg_instrs @ call_instr @ caller_saved_pop
    | DefSys (name, arg_typelist, ret_type) ->
      let arg_instrs = build_test_args arg_typelist args env fenv
      in let call_instr = 
        let n = List.length args in
        if n <= 6
          then [ ICall (Label name) ]
          else let x = Int64.of_int (8 * (n-6)) in
          [ ICall (Label name) ] @ [ IAdd (Reg RSP, Const x) ]
      in caller_saved_push @ arg_instrs @ call_instr @ caller_saved_pop @ tag_type ret_type @ test_type ret_type
    end
  | Tuple imm_list ->
    let n = List.length imm_list
    in let rec move_elems elems n =
      match elems with
      | [] -> []
      | hd::tl ->
        [ IComment ("store elem in slot " ^ (Int.to_string n))]
        @ compile_immexpr hd env fenv
        @ [ IMov (Qword (RegOffset (R15, n)), Reg RAX) ]
        @ move_elems tl (n+1)
    in
    [ IComment "store the size of the tuple in slot 0"]
    @ [ IMov (Qword (RegOffset (R15, 0)), Const (Int64.of_int n)) ]
    @ move_elems imm_list 1
    @ [ IMov (Reg RAX, Reg R15) ]
    @ tag_type (CTuple [])
    @ [ IAdd (Reg R15, Const(Int64.of_int (8 * (n+1)))) ] (* bump heap pointer *)
  | Set (t, k, v) ->
    compile_immexpr k env fenv (* index *)
    @ test_if_number
    @ untag_type CInt
    @ [ IMov (Reg R10, Reg RAX) ]
    @ compile_immexpr t env fenv (* tuple *)
    @ test_if_tuple
    @ compile_immexpr t env fenv (* restore tuple *)
    @ untag_type (CTuple [])
    @ [ IMov (Reg R11, RegOffset(RAX, 0)) ] (* get tuple size on R11 and check bounds*)
    @ [ ICmp (Reg R10, Reg R11) ]
    @ [ IJge (Label "error_index") ]
    @ [ ICmp (Reg R10, Const 0L) ]
    @ [ IJl  (Label "error_index") ]
    @ [ IAdd (Reg R10, Const 1L) ] (* finally set the value at idx+1 to skip over the size slot *)
    @ compile_immexpr v ~dst:R11 env fenv
    @ [ IMov (Qword (RegIndex (RAX, R10)), Reg R11) ] (* set the value *)
    @ tag_type (CTuple []) (* return the same tuple pointer *)
  | LamApply (lambda_expr, args) ->
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
    in let lambda = compile_immexpr lambda_expr env fenv in
    let arg_instrs = build_args args ~self:R10 env fenv in
    let args_len = List.length args in
    lambda
    @ test_if_closure
    @ [ IMov (Reg R10, Reg RAX) ]
    @ [ ISub (Reg RAX, Const closure_tag) ]
    @ [ IMov (Reg R11, Const (Int64.of_int args_len)) ]
    @ [ ICmp (Qword (RegOffset (RAX, 0)), Reg R11)]
    @ [ IJne (Label "error_wrong_arity") ]
    @ caller_saved_push
    @ arg_instrs
    @ [ ICall (RegOffset (RAX, 1)) ]
    @ caller_saved_pop
    @ (if args_len <= 6 
        then []
        else [ IAdd (Reg RSP, Const (Int64.of_int (8 * (args_len-6)))) ])

and compile_immexpr (expr:immexpr) ?(dst:reg = RAX) (env:env) (fenv:afenv)=
  match expr with
  | Num n ->
    if n > max_int || n < min_int
      then raise (CTError (sprintf "Integer overflow: " ^ (Int64.to_string n)))
      else [ IMov (Reg dst, Const (Int64.shift_left n 1))]
  | Bool true -> [ IMov (Reg dst, Const bool_true) ]
  | Bool false -> [ IMov (Reg dst, Const bool_false) ]
  | Id x -> 
    let reg, n = lookup_env x env in
    if n = 0
      then [ IMov (Reg dst, Reg reg) ] 
      else [ IMov (Reg dst, RegOffset (reg, n)) ]
  | Lambda (params, lambda_expr) -> 
    let lambda_label = Gensym.fresh "lambda_fun" in
    let free_vars = get_free_vars lambda_expr params in
    let arity = Int64.of_int (List.length params) in
    let depth = Int64.of_int (get_depth lambda_expr) in
    let n_free_vars = Int64.of_int (List.length free_vars) in
    let rec extend_env_rec lst env =
      match lst with
      | [] -> env
      | hd::tl -> let env', _ = extend_env hd RBP env in extend_env_rec tl env'
    in let lambda_env = extend_env_rec free_vars (gen_env ("self"::params) empty_env 0) in
    [ IJmp (Label (lambda_label^"_end")) ]
    (* Lambda Body *)
    @ [ ILabel (Label lambda_label) ]
    @ [ IPush (Reg RBP) ]
    @ [ IMov (Reg RBP, Reg RSP) ]
    @ [ ISub (Reg RSP, Const (Int64.mul n_free_vars 8L)) ]
    @ [ IMov (Reg R11, Reg RDI) ]
    @ [ ISub (Reg R11, Const closure_tag) ]
    @ List.fold_right (fun id instrs -> 
      let reg, slot = lookup_env id lambda_env in
      [ IMov (Reg RAX, Qword (RegOffset(R11, (List.length instrs)+3))) ]
      @ [ IMov (RegOffset(reg, slot), Reg RAX) ]
    ) free_vars []
    @ [ ISub (Reg RSP, Const (Int64.mul depth 8L)) ]
    @ compile_aexpr lambda_expr lambda_env fenv
    @ [ IMov (Reg RSP, Reg RBP) ]
    @ [ IPop (Reg RBP) ]
    @ [ IRet ]
    @ [ ILabel (Label (lambda_label^"_end")) ]
    (* Closure *)
    @ [ IMov (Qword (RegOffset (R15, 0)), Const arity) ]
    @ [ ILea (Reg R11, Rel (Label lambda_label)) ]
    @ [ IMov (Qword (RegOffset (R15, 1)), Reg R11) ]
    @ [ IMov (Qword (RegOffset (R15, 2)), Const n_free_vars) ]
    @ List.fold_right (fun id instrs -> 
      let reg, slot = lookup_env id env in
      [ IMov (Reg R10, RegOffset(reg, slot)) ]
      @ [ IMov (Qword (RegOffset(R15, (List.length instrs)+3)), Reg R10) ]
    ) free_vars []
    @ [ IMov (Reg dst, Reg R15) ]
    @ [ IAdd (Reg dst, Const closure_tag) ]
    @ [ IAdd (Reg R15, Const (Int64.mul (Int64.add n_free_vars 3L) 8L)) ]

and build_args args ?(self:reg option) env fenv =
  let rec inner_rec args instrs =
    match args with
    | [] -> instrs
    | hd::tl ->
      begin match List.length instrs with
      | 0 -> inner_rec tl (compile_immexpr hd ~dst:RDI env fenv @ instrs)
      | 1 -> inner_rec tl (compile_immexpr hd ~dst:RSI env fenv @ instrs)
      | 2 -> inner_rec tl (compile_immexpr hd ~dst:RDX env fenv @ instrs)
      | 3 -> inner_rec tl (compile_immexpr hd ~dst:RCX env fenv @ instrs)
      | 4 -> inner_rec tl (compile_immexpr hd ~dst:R8 env fenv @ instrs)
      | 5 -> inner_rec tl (compile_immexpr hd ~dst:R9 env fenv @ instrs)
      | n when n > 5 -> 
        begin match compile_immexpr hd env fenv with
        | [IMov (_, x)] -> inner_rec tl ([ IPush x ] @ instrs)
        | _ -> failwith "Unexpected behaviour"
        end
      | _ -> failwith "Negative list length"
      end
  in match self with
  | None -> inner_rec args []
  | Some r -> inner_rec args [IMov (Reg RDI, Reg r)]

(* Like build_args, but type checks each argument first *)
and build_test_args expected_types args env fenv =
  let rec inner_rec expected_types args instrs acount =
    match args with
    | [] -> instrs
    | hd::tl ->
      let expected_type = List.hd expected_types
      in let move_test_untag =
        compile_immexpr hd env fenv
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

and gen_env (params : string list) (env : env) (i:int) =
  match params with
  | [] -> env
  | hd::tl ->
    let i' = i+1 in
    match i with
    | 0 -> let env', _ = extend_env hd RDI env in gen_env tl env' i'
    | 1 -> let env', _ = extend_env hd RSI env in gen_env tl env' i'
    | 2 -> let env', _ = extend_env hd RDX env in gen_env tl env' i'
    | 3 -> let env', _ = extend_env hd RCX env in gen_env tl env' i'
    | 4 -> let env', _ = extend_env hd R8 env in gen_env tl env' i'
    | 5 -> let env', _ = extend_env hd R9 env in gen_env tl env' i'
    | k when k > 5 -> let env', _ = extend_env hd RBP ~fun_param:(k-4) env in gen_env tl env' i'
    | _ -> failwith "Negative list index"

let compile_afundefs (fenv: afundef list) : instruction list =
  let compile_afundef f fenv =
    match f with
    | DefFun (name, params, expr) ->
      [ ILabel (Label name) ]
      @ [ IPush (Reg RBP) ]
      @ [ IMov (Reg RBP, Reg RSP) ]
      @ [ ISub (Reg RSP, Const (Int64.of_int (8 * (get_depth expr)))) ]
      @ compile_aexpr expr (gen_env params empty_env 0) fenv
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
  let instrs = compile_aexpr aexpr empty_env afenv in
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
