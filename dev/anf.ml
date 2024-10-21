open Printf
open Ast
open Gensym

(* ANF representation library *)

type aexpr =
  | Let of string * cexpr * aexpr
  | If of immexpr * aexpr * aexpr
  | Ret of cexpr

and cexpr =
  | Atom of immexpr
  | Prim1 of prim1 * cexpr
  | Prim2 of prim2 * immexpr * immexpr
  | Apply of string * immexpr list
  | Tuple of immexpr list
  | Set of immexpr * immexpr * immexpr

and immexpr =
  | Num of int64
  | Bool of bool
  | Id of string

type afundef =
  | DefFun of string * string list * aexpr
  | DefSys of string * ctype list * ctype

let afundef_name (f : afundef) : string =
  match f with
  | DefFun (n, _, _) -> n
  | DefSys (n, _, _) -> n

let rec anf_aexpr (expr : expr) : aexpr =
  match expr with
  | Num n -> Ret (Atom (Num n))
  | Bool b -> Ret (Atom (Bool b))
  | Id x -> Ret (Atom (Id x))
  | Prim1 (op, expr) ->
    anf_c expr (fun c_expr -> Ret (Prim1 (op, c_expr)))
  | Prim2 (op, expr1, expr2) ->
    anf_imm expr1 (fun imm_expr1 ->
      anf_imm expr2 (fun imm_expr2 -> Ret (Prim2 (op, imm_expr1, imm_expr2))))
  | Let (id, bound_expr, body_expr) ->
    anf_imm bound_expr (fun imm_expr ->
      Let (id, Atom imm_expr, anf_aexpr body_expr))
  | If (cond_expr, t_expr, f_expr) ->
    anf_imm cond_expr (fun imm_expr ->
      If (imm_expr, anf_aexpr t_expr, anf_aexpr f_expr))
  | Apply (name, expr_l) ->
    let acc_let expr ctx vs =
      anf_imm expr (fun imm_expr -> ctx (imm_expr :: vs))
    in
    let base vs =
      Ret (Apply (name, List.rev vs))
    in
    List.fold_right acc_let expr_l base []
  | Tuple expr_list ->
    let accum expr ctx vs =
      anf_imm expr (fun imm_expr -> ctx (imm_expr :: vs))
    in let base vs =
      Ret (Tuple (List.rev vs))
    in List.fold_right accum expr_list base []
  | Set (e1, e2, e3) ->
    anf_imm e1 (fun imm_expr1 ->
      anf_imm e2 (fun imm_expr2 ->
        anf_imm e3 (fun imm_expr3 ->
          Ret (Set (imm_expr1, imm_expr2, imm_expr3)))))


and anf_imm (expr : expr) (k : immexpr -> aexpr) : aexpr =
  match expr with
  | Num n -> k (Num n)
  | Bool b -> k (Bool b)
  | Id x -> k (Id x)
  | Prim1 (op, expr) ->
    let tmp = Gensym.fresh (
      match op with
      | Add1 -> "add1"
      | Sub1 -> "sub1"
      | Not -> "not"
      | Print -> "print") in
    anf_c expr (fun c_expr -> 
      Let (tmp, Prim1 (op, c_expr), k (Id tmp)))
  | Prim2 (op, expr1, expr2) ->
    let tmp = Gensym.fresh (
      match op with
      | Add -> "add"
      | Lte -> "lte"
      | And -> "and"
      | Or -> "or"
      | Get -> "get") in
    anf_imm expr1 (fun imm_expr1 ->
      anf_imm expr2 (fun imm_expr2 ->
        Let (tmp, Prim2 (op, imm_expr1, imm_expr2), k (Id tmp))))
  | Let (id, bound_expr, body_expr) ->
    anf_imm bound_expr (fun imm_expr ->
      Let (id, Atom imm_expr, anf_imm body_expr k))
  | If (cond_expr, then_expr, else_expr) ->
    anf_imm cond_expr (fun imm_expr ->
      If (imm_expr, anf_imm then_expr k , anf_imm else_expr k))
  | Apply (name, expr) -> 
    let acc_let expr ctx vs =
      anf_imm expr (fun imm_expr -> ctx (imm_expr :: vs))
    in
    let base vs =
      let id = Gensym.fresh name in
      Let (id, Apply (name, List.rev vs), k (Id id))
    in
    List.fold_right acc_let expr base []
  | Tuple expr_list ->
    let accum expr ctx vs =
      anf_imm expr (fun imm_expr -> ctx (imm_expr :: vs))
    in let base vs =
      let tmp = Gensym.fresh "tup" in
      Let (tmp, Tuple (List.rev vs), k (Id tmp))
    in List.fold_right accum expr_list base []
  | Set (e1, e2, e3) ->
    let tmp = Gensym.fresh "set" in
    anf_imm e1 (fun imm_expr1 ->
      anf_imm e2 (fun imm_expr2 ->
        anf_imm e3 (fun imm_expr3 ->
          Let (tmp, Set (imm_expr1, imm_expr2, imm_expr3), k (Id tmp)))))
          
and anf_c (expr: expr) (k : cexpr -> aexpr) : aexpr =
  match expr with
  | Num n -> k (Atom (Num n))
  | Bool b -> k (Atom (Bool b))
  | Id x -> k (Atom (Id x))
  | Prim1 (op, expr) ->
    anf_c expr (fun c_expr ->
      k (Prim1 (op, c_expr)))
  | Prim2 (op, expr1, expr2) ->
    anf_imm expr1 (fun imm_expr1 ->
      anf_imm expr2 (fun imm_expr2 ->
        k (Prim2 (op, imm_expr1, imm_expr2))))
  | Let (id, bound_expr, body_expr) ->
    anf_c bound_expr (fun c_expr ->
    Let (id, c_expr, anf_c body_expr k))
  | If (cond_expr, then_expr, else_expr) ->
    anf_imm cond_expr (fun imm_expr ->
      If (imm_expr, anf_c then_expr k, anf_c else_expr k))
  | Apply (name, expr_list) ->
    let acc_let expr ctx vs =
      anf_imm expr (fun imm_expr -> ctx (imm_expr :: vs))
    in let base vs =
      let id = Gensym.fresh name in
      Let (id, Apply (name, List.rev vs), k (Atom (Id id)))
    in
    List.fold_right acc_let expr_list base []
  | Tuple expr_list ->
    let accum expr ctx vs =
      anf_imm expr (fun imm_expr -> ctx (imm_expr :: vs))
    in let base vs =
      k (Tuple (List.rev vs))
    in List.fold_right accum expr_list base []
  | Set (e1, e2, e3) ->
    anf_imm e1 (fun imm_expr1 ->
      anf_imm e2 (fun imm_expr2 ->
        anf_imm e3 (fun imm_expr3 ->
          k (Set (imm_expr1, imm_expr2, imm_expr3)))))

let anf_expr (expr : expr) : aexpr =
  let masked_expr = alpha_rename_expr expr Env.empty in
  anf_aexpr masked_expr

let anf_fundef (f : fundef) : afundef =
  match f with
  | DefSys (n, l, r) -> DefSys (n, l, r)
  | DefFun (n, l, e) ->
    let l', env = gen_env l n in
    let masked_expr = alpha_rename_expr e env in
    DefFun (n, l', anf_aexpr masked_expr)

let get_depth aexpr =
  let rec count aexpr acc =
    match aexpr with
    | Ret _ -> acc
    | Let (_, _, aexpr) -> count aexpr (acc + 1)
    | If (_, t_branch, f_branch) -> max (count t_branch acc) (count f_branch acc)
  in count aexpr 0

let rec string_of_aexpr (a : aexpr) : string =
  match a with
  | Let (id, c, a) -> 
    sprintf "(let (%s %s) %s)" 
    id (string_of_cexpr c) (string_of_aexpr a)
  | Ret c -> sprintf "(ret %s)" (string_of_cexpr c)
  | If (icond, athen, aelse) -> sprintf "(if %s %s %s)" 
    (string_of_immexpr icond) 
    (string_of_aexpr athen)
    (string_of_aexpr aelse)

and string_of_cexpr (c : cexpr) : string =
  match c with
  | Atom i -> string_of_immexpr i
  | Prim1 (op, c) -> sprintf "(%s %s)"
    (match op with
    | Add1 -> "add1"
    | Sub1 -> "sub1"
    | Not -> "not"
    | Print -> "print") (string_of_cexpr c)
  | Prim2 (op, i1, i2) -> sprintf "(%s %s %s)"
    (match op with
    | Add -> "+"
    | And -> "and"
    | Or -> "or"
    | Lte -> "<="
    | Get -> "get") (string_of_immexpr i1) (string_of_immexpr i2)
  | Apply (name, args) -> sprintf "(%s (%s))"
    name
    (String.concat " " (List.map string_of_immexpr args))
  | Tuple expr_list -> sprintf ("(tup %s)") (String.concat " " (List.map string_of_immexpr expr_list))
  | Set (e1, e2, e3) -> sprintf ("(set %s %s %s)") (string_of_immexpr e1) (string_of_immexpr e2) (string_of_immexpr e3)

and string_of_immexpr (i : immexpr) : string =
  match i with
  | Num n -> Int64.to_string n
  | Bool b -> if b then "true" else "false"
  | Id s -> s

let string_of_afundef(d : afundef) : string =
  match d with
  | DefFun (name, arg_ids, body) -> sprintf "(def (%s %s) %s)" name (String.concat " " arg_ids) (string_of_aexpr body)
  | DefSys (name, arg_types, ret_type) -> sprintf "(defsys %s %s -> %s)" name (String.concat " " (List.map string_of_ctype arg_types)) (string_of_ctype ret_type)