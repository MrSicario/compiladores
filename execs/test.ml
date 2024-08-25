open Dev.Ast
open Dev.Parse
open Dev.Interp
open Dev.Lib
open Alcotest

(* Testing arithmetic expression using the print function defined in Interp 
   and the default equality for comparison *)
let exp : exp testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_exp e)) (=)

let value : value testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_val e)) (=)


(* Tests for our [parse] function *)
let test_parse_int () =
  check exp "same int" (parse (`Atom "5")) (Num 5)

let test_parse_var () =
  check exp "same var" (parse (`Atom "x")) (Var "x")

let test_parse_compound () =
  check exp "same expr"
    (parse (`List [`Atom "+" ; `List [`Atom "*" ; `Atom "3"; `Atom "x"]; `Atom "7"]))
    (Plus (Times (Num 3, Var "x"), Num 7))

let test_parse_error () =
  let sexp = `List [`Atom "foo"; `Atom "bar"] in
  check_raises "Should raise failwith" 
    (Failure (Fmt.str "Not a valid exp: %a" CCSexp.pp sexp))
    (fun () -> ignore @@ parse sexp)

(* Tests for our [interp] function *)
let test_interp_num () =
  check value "same int" (interp empty_env (Num 42)) (NumV 42)

let test_interp_var () =
  check value "same int" (interp ["x", NumV 7] (Var "x")) (NumV 7)

let test_interp_compound () =
  check value "same int"
    (interp empty_env (Plus (Times (Num 3, Num 5), Num 12)))
    (NumV 27)

(* Tests for our [simplify] function *)
let test_simplify_no_match () =
  check exp "same exp" (Plus (Times (Num 3, Var "x"), Num 7)) (Plus (Times (Num 3, Var "x"), Num 7))

let test_simplify_times_right_int () =
  check exp "same int" (simplify (Times (Num 3, Num 1))) (Num 3)

let test_simplify_times_left_int () =
  check exp "same int" (simplify (Times (Num 1, Num 3))) (Num 3)

let test_simplify_times_right_var () =
  check exp "same var" (simplify (Times (Var "x", Num 1))) (Var "x")

let test_simplify_times_left_var () =
  check exp "same var" (simplify (Times (Num 1, Var "x"))) (Var "x")

let test_simplify_times_right_cmp () =
  check exp "same expr" (simplify (Times ((Plus (Times (Num 3, Var "x"), Num 7)), Num 1)))
  (Plus (Times (Num 3, Var "x"), Num 7))

let test_simplify_times_left_cmp () =
  check exp "same expr" (simplify (Times (Num 1, (Plus (Times (Num 3, Var "x"), Num 7))))) 
  (Plus (Times (Num 3, Var "x"), Num 7))

let test_simplify_plus_right_int () =
  check exp "same int" (simplify (Plus (Num 3, Num 0))) (Num 3)

let test_simplify_plus_left_int () =
  check exp "same int" (simplify (Plus (Num 0, Num 3))) (Num 3)

let test_simplify_plus_right_var () =
  check exp "same var" (simplify (Plus (Var "x", Num 0))) (Var "x")

let test_simplify_plus_left_var () =
  check exp "same var" (simplify (Plus (Num 0, Var "x"))) (Var "x")

let test_simplify_plus_right_cmp () =
  check exp "same expr" (simplify (Plus ((Plus (Times (Num 3, Var "x"), Num 7)), Num 0)))
  (Plus (Times (Num 3, Var "x"), Num 7))

let test_simplify_plus_left_cmp () =
  check exp "same expr" (simplify (Plus (Num 0, (Plus (Times (Num 3, Var "x"), Num 7))))) 
  (Plus (Times (Num 3, Var "x"), Num 7))

(* Entry point of tests
 * Beware that the [Alcotest] library takes control of all command line
 * arguments in [run].
 * See the documentation at https://github.com/mirage/alcotest *)
let () =
  run "A Simple Interpreter"
    [
      "parse", [
        test_case "A number" `Quick test_parse_int ;
        test_case "A variable" `Quick test_parse_var ;
        test_case "A compound expression" `Quick test_parse_compound ;
        test_case "An invalid s-expression" `Quick test_parse_error
      ] ;

      "interp", [
        test_case "A number" `Quick test_interp_num ;
        test_case "A variable" `Quick test_interp_var ;
        test_case "A compound expression" `Quick test_interp_compound
      ] ;

      "simplify", [
        test_case "Expression can't be simplified" `Quick test_simplify_no_match ;
        test_case "(Num * 1) -> Num" `Quick test_simplify_times_right_int ;
        test_case "(1 * Num) -> Num" `Quick test_simplify_times_left_int ; 
        test_case "(Var * 1) -> Var" `Quick test_simplify_times_right_var ;
        test_case "(1 * Var) -> Var" `Quick test_simplify_times_left_var ; 
        test_case "(expr * 1) -> expr" `Quick test_simplify_times_right_cmp ;
        test_case "(1 * expr) -> expr" `Quick test_simplify_times_left_cmp ;
        test_case "(Num + 0) -> Num" `Quick test_simplify_plus_right_int ;
        test_case "(0 + Num) -> Num" `Quick test_simplify_plus_left_int ; 
        test_case "(Var + 0) -> Var" `Quick test_simplify_plus_right_var ;
        test_case "(0 + Var) -> Var" `Quick test_simplify_plus_left_var ; 
        test_case "(expr + 0) -> expr" `Quick test_simplify_plus_right_cmp ;
        test_case "(0 + expr) -> expr" `Quick test_simplify_plus_left_cmp ; 
      ]
    ]