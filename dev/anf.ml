open Printf
open Ast
open Gensym

(* ANF representation library *)

type aexpr =
  | Let of string * cexpr * aexpr
  | Ret of cexpr

and cexpr =
  | Atom of immexpr
  | Prim1 of prim1 * cexpr
  | Prim2 of prim2 * immexpr * immexpr
  | If of immexpr * cexpr * cexpr

and immexpr =
  | Num of int64
  | Bool of bool
  | Id of string

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
      anf_c t_expr (fun c_expr1 ->
        anf_c f_expr (fun c_expr2 ->
          Ret (If (imm_expr, c_expr1, c_expr2)))))
  | Apply (_, _) -> failwith "To Be Done"
      
and anf_imm (expr : expr) (k : immexpr -> aexpr) : aexpr =
  match expr with
  | Num n -> k (Num n)
  | Bool b -> k (Bool b)
  | Id x -> k (Id x)
  | Prim1 (op, expr) ->
    let tmp = Gensym.fresh "prim1" in
    anf_c expr (fun c_expr -> 
      Let (tmp, Prim1 (op, c_expr), k (Id tmp)))
  | Prim2 (op, expr1, expr2) ->
    let tmp = Gensym.fresh "prim2" in
    anf_imm expr1 (fun imm_expr1 ->
      anf_imm expr2 (fun imm_expr2 ->
        Let (tmp, Prim2 (op, imm_expr1, imm_expr2), k (Id tmp))))
  | Let (id, bound_expr, body_expr) ->
    anf_imm bound_expr (fun imm_expr ->
      Let (id, Atom imm_expr, anf_imm body_expr (fun imm_expr -> k imm_expr)))
  | If (cond_expr, then_expr, else_expr) ->
    let tmp = Gensym.fresh "if" in
    anf_imm cond_expr (fun imm_expr ->
      anf_c then_expr (fun c_expr1 ->
        anf_c else_expr (fun c_expr2 ->
          Let (tmp, If (imm_expr, c_expr1, c_expr2), k (Id tmp)))))
  | Apply (_, _) -> failwith "To Be Done"
          
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
    Let (id, c_expr, anf_c body_expr (fun c_expr -> k c_expr)))
  | If (cond_expr, then_expr, else_expr) ->
    anf_imm cond_expr (fun imm_expr ->
      anf_c then_expr (fun c_expr1 ->
        anf_c else_expr (fun c_expr2 ->
          k (If (imm_expr, c_expr1, c_expr2)))))
  | Apply (_, _) -> failwith "To Be Done"
          
let rec string_of_aexpr (a : aexpr) : string =
  match a with
  | Let (id, c, a) -> 
    sprintf "(let (%s %s) %s)" 
    id (string_of_cexpr c) (string_of_aexpr a)
  | Ret c -> sprintf "(ret %s)" (string_of_cexpr c)

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
    | Lte -> "<=") (string_of_immexpr i1) (string_of_immexpr i2)
  | If (icond, cthen, celse) -> sprintf "(if %s %s %s)" 
    (string_of_immexpr icond) 
    (string_of_cexpr cthen)
    (string_of_cexpr celse)

and string_of_immexpr (i : immexpr) : string =
  match i with
  | Num n -> Int64.to_string n
  | Bool b -> if b then "true" else "false"
  | Id s -> s
  
  let anf (expr : expr) : aexpr =
    let masked_expr = alpha_rename_expr expr Env.empty in
      anf_aexpr masked_expr