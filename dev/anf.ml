open Printf
open Ast

type aexpr =
  | Let of string * cexpr * aexpr
  | Ret of cexpr

and cexpr =
  | Atom of immexpr
  | Prim1 of prim1 * cexpr
  | Prim2 of prim2 * immexpr * immexpr

and immexpr =
  | Num of int64
  | Bool of bool
  | Id of string

let gensym : string -> string =
  let count = ref 0 in
  fun base: string ->
    count := !count + 1;
    sprintf "tmp_%s_%d" base !count

let rec scram (expr : expr) (old_bind : string) (new_bind : string) : expr =
  match expr with
  | Num n -> Num n
  | Bool b -> Bool b
  | Id x -> if x = old_bind then Id new_bind else Id x
  | Prim1 (op, expr) -> Prim1 (op, (scram expr old_bind new_bind))
  | Prim2 (op, expr1, expr2) -> 
    Prim2 (op, (scram expr1 old_bind new_bind), (scram expr2 old_bind new_bind))
  | Let (id, bound_expr, body_expr) ->
    Let (id, (scram bound_expr old_bind new_bind), (scram body_expr old_bind new_bind))
  | If (cond_expr, then_expr, else_expr) ->
    If ((scram cond_expr old_bind new_bind),
        (scram then_expr old_bind new_bind),
        (scram else_expr old_bind new_bind))

let rec anf (expr : expr) : aexpr =
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
    let scram_id = gensym id in
    anf_imm bound_expr (fun imm_expr ->
      Let (scram_id, Atom imm_expr, anf (scram body_expr id scram_id)))
  | _ -> failwith "Not yet implemented" 
      
and anf_imm (expr : expr) (k : immexpr -> aexpr) : aexpr =
  match expr with
  | Num n -> k (Num n)
  | Bool b -> k (Bool b)
  | Id x -> k (Id x)
  | Prim1 (op, expr) ->
    let tmp = gensym "prim1" in
    anf_c expr (fun c_expr -> 
      Let (tmp, Prim1 (op, c_expr), k (Id tmp)))
  | Prim2 (op, expr1, expr2) ->
    let tmp = gensym "prim2" in
    anf_imm expr1 (fun imm_expr1 ->
      anf_imm expr2 (fun imm_expr2 ->
        Let (tmp, Prim2 (op, imm_expr1, imm_expr2), k (Id tmp))))
  | Let (id, bound_expr, body_expr) ->
    let scram_id = gensym id in
    anf_imm bound_expr (fun imm_expr ->
      Let (scram_id, Atom imm_expr, anf_imm (scram body_expr id scram_id) (fun imm_expr -> k imm_expr)))
  | _ -> failwith "Not yet implemented"

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
    let scram_id = gensym id in
    anf_c bound_expr (fun c_expr ->
      Let (scram_id, c_expr, anf_c (scram body_expr id scram_id) (fun c_expr -> k c_expr)))
  | _ -> failwith "Not yet implemented"

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
    | Sub1 -> "sub1") (string_of_cexpr c)
  | Prim2 (op, i1, i2) -> sprintf "(%s %s %s)"
    (match op with
    | Add -> "+"
    | And -> "and"
    | Lte -> "<=") (string_of_immexpr i1) (string_of_immexpr i2)

and string_of_immexpr (i : immexpr) : string =
  match i with
  | Num n -> Int64.to_string n
  | Bool b -> if b then "true" else "false"
  | Id s -> s