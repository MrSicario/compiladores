open Dev.Ast
open Dev.Parse
open Dev.Interp
open Dev.Compile
open Alcotest
open Bbctester.Test
open Printf

(* Testing arithmetic expression using the print function defined in Interp 
   and the default equality for comparison *)
let exp : expr testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_expr e)) (=)

let value : value testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_val e)) (=)


(* Tests for our [parse] function *)
let test_parse_int () =
  check exp "same int" (parse_exp (`Atom "5")) (Num 5L)

let test_parse_int_neg () =
  check exp "same int" (parse_exp (`Atom "-3")) (Num (-3L))

let test_parse_var () =
  check exp "same var" (parse_exp (`Atom "x")) (Id "x")

let test_parse_compound () =
  check exp "same expr"
    (parse_exp (`List [`Atom "+" ; `List [`Atom "+" ; `Atom "3"; `Atom "x"]; `Atom "7"]))
    (Prim2 (Add, Prim2 (Add, Num 3L, Id "x"), Num 7L))

let test_parse_error () =
  let sexp = `List [`Atom "foo"; `Atom "bar"] in
  check_raises "Should raise failwith" 
    (Failure (Fmt.str "Not a valid expr: %a" CCSexp.pp sexp))
    (fun () -> ignore @@ parse_exp sexp)

let test_parse_let () =
  check exp "same expr"
    (parse_exp (`List [`Atom "let" ; `List [`Atom "var" ; `List [`Atom "+" ; `Atom "2" ; `Atom "7"]] ; `Atom "var"]))
    (Let ("var", Prim2 (Add, Num 2L, Num 7L), Id "var"))

let test_parse_let_nested () =
  check exp "same expr"
    (parse_exp (`List [`Atom "let" ; `List [`Atom "x" ; `Atom "17"] ; `List [`Atom "let" ; `List [`Atom "y" ; `Atom "11"] ; `Atom "y"]]))
    (Let ("x", Num 17L, Let ("y", Num 11L, Id "y")))

(* Tests for our [interp] function *)
let test_interp_num () =
  check value "same int" (interp (Num 42L) empty_env) (NumV 42L)

let test_interp_var () =
  check value "same int" (interp (Id "x") ["x", NumV 7L]) (NumV 7L)

let test_interp_compound () =
  check value "same int"
    (interp (Prim2 (Add, Prim2 (Add, Num 3L, (Prim1 (Sub1, Num 6L))), Num 12L)) empty_env)
    (NumV 20L)

let test_interp_let () =
  check value "same int"
    (interp (Let ("a", Num 9L, Prim1 (Sub1, Id "a"))) empty_env)
    (NumV 8L)

let test_interp_let_nested () =
  check value "same int"
    (interp (Let ("x", Prim1 (Add1, Num 3L), Let ("y", Num 14L, Prim2 (Add, Id "x", Id "y")))) empty_env)
    (NumV 18L)

(* todo: a better error message for unknown identifiers *)
let test_interp_let_unknownid () =
  let sexp = (Let ("x", Num 3L, Id "y")) in
  check_raises "Should raise Not_found"
    (Not_found)
    (fun () -> ignore @@ (interp sexp empty_env))

let test_interp_if () =
  check value "same int"
    (interp (If (Prim2 (And, Bool true, Bool false), Num 3L, Num 22L)) empty_env)
    (NumV 22L)

let test_interp_if_error () =
  let sexp = (If (Num 3L, Num 7L, Num 11L)) in
  check_raises "Should raise runtime type error"
    (Failure "runtime type error")
    (fun () -> ignore @@ (interp sexp empty_env))

let test_interp_if_brancheval () =
  check value "same int"
    (interp (If (Bool true, Num 6L, Prim2 (And, Num 2L, Num 5L))) empty_env)
    (NumV 6L)

(* OCaml tests: extend with your own tests *)
let ocaml_tests = [
  "parse", [
    test_case "A number" `Quick test_parse_int ;
    test_case "A negative number" `Quick test_parse_int_neg ;
    test_case "A variable" `Quick test_parse_var ;
    test_case "A compound expression" `Quick test_parse_compound ;
    test_case "An invalid s-expression" `Quick test_parse_error ;
    test_case "A let expression" `Quick test_parse_let ;
    test_case "A nested let expression" `Quick test_parse_let_nested ;
  ] ;
  "interp", [
    test_case "A number" `Quick test_interp_num ;
    test_case "A variable" `Quick test_interp_var ;
    test_case "A compound expression" `Quick test_interp_compound ;
    test_case "A let binding" `Quick test_interp_let ;
    test_case "A nested let binding" `Quick test_interp_let_nested ;
    test_case "An unknown identifier" `Quick test_interp_let_unknownid ;
    test_case "An if expression" `Quick test_interp_if ;
    test_case "An invalid if expression" `Quick test_interp_if_error ;
    test_case "A non-evaluated expression of invalid type" `Quick test_interp_if_brancheval ;
  ]
]

(* Entry point of tester *)
let () =
  (* BBC tests: don't change the following, simply add .bbc files in the bbctests/ directory *)
  let bbc_tests = 
    let compile_flags = Option.value (Sys.getenv_opt "CFLAGS") ~default:"-g" in
    let compiler : string -> out_channel -> unit = 
      fun s o -> fprintf o "%s" (compile (parse_exp (sexp_from_string s))) in
    let oracle : string -> status * string = 
      fun s -> NoError , string_of_val (interp (parse_exp (sexp_from_string s)) empty_env) in
    tests_from_dir ~compile_flags ~compiler ~oracle ~runtime:"rt/sys.c" "bbctests" in
  run "Tests entrega 1" (ocaml_tests @ bbc_tests)
