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
    Printf.sprintf "tmp_%s_%d" base !count

let rec anf (expr : expr) : aexpr =
  match expr with
  | Num n -> Ret (Atom (Num n))
  | Bool b -> Ret (Atom (Bool b))
  | Id x -> Ret (Atom (Id x))
  | Prim1 (op, expr) ->
    anf_c expr (fun c_expr -> Ret (Prim1 (op, c_expr)))
  | Prim2 (op, expr1, expr2) ->
    anf_imm expr1 (fun imm_expr1 ->
      anf_imm expr2 (fun imm_expr2 ->
        Ret (Prim2 (op, imm_expr1, imm_expr2))))
  | Let (id, bound_expr, body_expr) ->
    anf_imm bound_expr (fun imm_expr ->
      Let (id, Atom imm_expr, anf body_expr))
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
    anf_imm bound_expr (fun imm_expr ->
      Let (id, Atom imm_expr, anf_imm body_expr (fun imm_expr -> k imm_expr)))
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
    anf_c bound_expr (fun c_expr ->
      Let (id, c_expr, anf_c body_expr (fun c_expr -> k c_expr)))
  | _ -> failwith "Not yet implemented"