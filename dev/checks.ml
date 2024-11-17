open Anf
open Parse
open Printf

(* Function Environment *)
type afenv = afundef list
let empty_afenv : afenv = []
let rec lookup_afenv : string -> afenv -> afundef =
  fun s fenv -> 
    match fenv with
    | [] -> raise (CTError (Printf.sprintf "Undefined function: %s" s))
    | (f::fs) -> if afundef_name f = s then f else lookup_afenv s fs

(* A-expr structure check & function arity check *)
let rec check_aexpr e afenv =
  match e with
  | Let (_, bound_expr, body_expr) ->
    check_cexpr bound_expr afenv && check_aexpr body_expr afenv
  | LetRec (_, body_expr) ->
    check_aexpr body_expr afenv
  | If (_, then_expr, else_expr) ->
    check_aexpr then_expr afenv && check_aexpr else_expr afenv
  | Ret (cexpr) -> check_cexpr cexpr afenv

and check_cexpr e afenv =
  match e with
  | Atom _ -> true
  | Prim1 (_, cexpr) -> check_cexpr cexpr afenv
  | Prim2 (_, _, _) -> true
  | Apply (f, args) ->
    begin match lookup_afenv f afenv with
    | DefFun (_, vars, _) ->
      let var_l = List.length vars in
      let arg_l = List.length args in
      if var_l = arg_l 
        then true
      else raise (CTError 
        (sprintf "Arity mismatch: %s expected %s arguments but got %s"
        f (string_of_int var_l) (string_of_int arg_l)))
    | DefSys (_, vars, _) -> 
      let var_l = List.length vars in
      let arg_l = List.length args in
      if var_l = arg_l 
        then true
      else raise (CTError 
        (sprintf "Arity mismatch: %s expected %s arguments but got %s"
        f (string_of_int var_l) (string_of_int arg_l)))
    end
  | LamApply (_, _) -> true
  | Tuple _ -> true
  | Set (_, _, _) -> true
  
let check_anf aexpr afenv =
  let _ = check_aexpr aexpr afenv in
  aexpr
  
let check_afundefs afundefs =
  let rec check_declaration global ls names =
    match ls with
    | [] -> true
    | hd::tl ->
      begin match hd with
      | DefFun (n, _, e) ->
        if List.mem n names then
          raise (CTError (sprintf "Function already declared in namespace: %s" n))
        else 
          let names' = n::names in
          if check_aexpr e global then
            check_declaration global tl names'
          else false
      | DefSys (n, _, _) ->
        if List.mem n names then
          raise (CTError (sprintf "Function already declared in namespace: %s" n))
        else
          let names' = n::names in
          check_declaration global tl names'
      end
  in let _ = check_declaration afundefs afundefs [] in
  afundefs