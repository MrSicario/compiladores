(** Lib of runtime structures (values, environments) **)
open Printf
open Ast

(** Values **)
type value = 
  | NumV of int
  | BoolV of bool
  | ClosureV of string * exp * ((string * value) list)

(* Pretty printing *)
let string_of_val(v : value) : string =
  match v with
  | NumV n -> string_of_int n
  | BoolV b -> string_of_bool b
  | ClosureV (id, _, _) -> sprintf "(Closure of (%s))" id

(* Lifting functions on int to operate on values *)
let liftNumV : (int -> int -> int) -> value -> value -> value =
  fun op e1 e2 ->
    match e1, e2 with
    | NumV n1, NumV n2 -> NumV (op n1 n2)
    | _ -> failwith (sprintf "Not a NumV")

let liftNumVBoolV : ('a -> 'a -> bool) -> value -> value -> value =
  fun op -> fun e1 e2 ->
    match e1, e2 with
    | NumV n1, NumV n2 -> BoolV (op n1 n2)
    | _ -> failwith (sprintf "Not a NumV")

(* Lifting functions on booleans to operate on values*)
let liftBoolV : (bool -> bool -> bool) -> value -> value -> value =
  fun op e1 e2 -> 
    match e1, e2 with
    | BoolV b1, BoolV b2 -> BoolV (op b1 b2)
    | _ -> failwith (sprintf "Not a BoolV")

(** Environments **)

(* Type alias
  ['a list] is the type of expressions returning a list of some type 'a
  Here environment as lists of pairs (i.e. association lists)
*)
type env = (string * value) list

let empty_env : env = []

let extend_env : string -> value -> env -> env =
  fun x v env -> (x, v) :: env
