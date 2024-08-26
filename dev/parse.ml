(** Parser **)

open Ast
open Printf
(* we use CCsexp as a library for s-expressions *)
open CCSexp

(*
  Instead of using a standard algebraic data types for sexps, such as:
  
      type sexp = Atom of string | List of sexp list 
 
  this library uses a feature known as "polymorphic variants".
  This is a flexible mechanism where data is built by tagging it with symbols,
  instead of using pre-declared constructors. These symbols are written with ticks `.
  
  Then sexp is just an alias for a (recursive) polymorphic variant:

    type sexp = [ `Atom of string | `List of 'a list ] as 'a

  When matching such an sexp, we look at the tag. See the parse function below for an example.
  You can also look at the definition and implementation of the CCsexp module for more details.
*)

let rec parse (sexp : sexp) : exp = (* it's not a problem to have a variable [sexp] of type [sexp] (separate namespaces) *)
  match sexp with
  | `Atom s ->
    begin
      match int_of_string_opt s with     (* A frequent trouble: when nesting [match] the inner match must *)
      | Some n  -> Num n                 (* be enclosed in [( ... )] or [begin ... end] *)
      | None -> 
        begin
          match bool_of_string_opt s with
          | Some b -> Bool b
          | None -> Var s
        end
    end
  | `List [`Atom "+" ; e1 ; e2 ] -> Plus (parse e1, parse e2)
  | `List [`Atom "-" ; e1 ; e2 ] -> Sub (parse e1, parse e2)
  | `List [`Atom "*" ; e1 ; e2 ] -> Times (parse e1, parse e2)
  | `List [`Atom "/" ; e1 ; e2 ] -> Div (parse e1, parse e2)
  | `List [`Atom "=" ; e1 ; e2 ] -> Eq (parse e1, parse e2)
  | `List [`Atom "<" ; e1 ; e2 ] -> Lt (parse e1, parse e2)
  | `List [`Atom "and" ; e1 ; e2 ] -> And (parse e1, parse e2)
  | `List [`Atom "if" ; e1 ; e2 ; e3 ] -> If (parse e1, parse e2, parse e3)
  | `List [`Atom "fun" ; `List [`Atom id] ; body ] -> Fun (id, parse body)
  | `List [`Atom "let" ; `List [`Atom id ; bind] ; body ] -> App ((Fun (id, parse body)), parse bind)
  | `List [`Atom "let*" ; `List binds ; body] ->
    let rec parse_lets l base =
      match l with
      | [] -> parse base
      | (`List [`Atom id ; bind]) :: t -> App ((Fun (id, (parse_lets t base))), parse bind)
      | _ -> failwith "'let*' syntax error"
    in parse_lets binds body
  | `List [e1 ; e2] -> App (parse e1, parse e2)
  | _ -> failwith (sprintf "Not a valid exp: %s" (to_string sexp))

let sexp_from_file : string -> CCSexp.sexp =
  fun filename ->
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> failwith (sprintf "Unable to parse file %s: %s" filename msg)
