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

let test_parse_bool () =
  check exp "same bool" (parse (`Atom "true")) (Bool true)

let test_parse_var () =
  check exp "same var" (parse (`Atom "x")) (Var "x")

let test_parse_fun () =
  check exp "same fun" (parse (`List [`Atom "fun" ; `List [`Atom "x"] ; `List [`Atom "+" ; `Atom "5" ; `Atom "x" ]]))
  (Fun ("x", Plus (Num 5, Var "x")))
  
let test_parse_app () =
  let sexp = `List [`List [`Atom "fun" ; `List [`Atom "x"] ; `Atom "x" ] ; `Atom "3"] in
  check exp "same app" (parse sexp) (App ((Fun ("x", Var "x")), Num 3))

let test_parse_let () =
  let sexp = `List [`Atom "let" ; `List [ `Atom "x" ; `Atom "3" ] ; `Atom "x"] in
  check exp "proper sintax sugar" (parse sexp) (App (Fun ("x", Var "x"), Num 3))

let test_parse_lets () =
  let sexp = `List [`Atom "let*" ; 
  `List [
    `List [ `Atom "x" ; `Atom "1" ] ;
    `List [ `Atom "y" ; `Atom "2" ]
    ] ; 
  `List [`Atom "+" ; `Atom "y" ; `Atom "x"]] in
  check exp "proper sintax sugar" (parse sexp)
  (App (Fun ("x", (App ((Fun ("y", (Plus (Var "y", Var "x")))), Num 2))), Num 1))

let test_parse_compound () =
  check exp "same expr"
    (parse (`List [`Atom "+" ; `List [`Atom "*" ; `Atom "3"; `Atom "x"] ; `Atom "7"]))
    (Plus (Times (Num 3, Var "x"), Num 7))

(* Tests for our [interp] function *)
let test_interp_num () =
  check value "same int" (interp empty_env (Num 42)) (NumV 42)

let test_interp_var () =
  check value "same int" (interp ["x", NumV 7] (Var "x")) (NumV 7)

let test_interp_bool () =
  check value "same bool" (interp empty_env (Bool true)) (BoolV true)

let test_interp_compound () =
  check value "same int"
    (interp empty_env (Div (Sub (Plus (Times (Num 3, Num 5), Num 12), Num 1), Num 2)))
    (NumV 13)

let test_interp_and () =
  check value "same bool" (interp empty_env (And (Bool true, Bool false))) (BoolV false)

let test_interp_and_shortcircuit () =
  check value "same bool" (interp empty_env (And (Bool false, Num 1))) (BoolV false)

let test_interp_if_true () =
  check value "same int" (interp empty_env (If (Bool true, Num 1, Num 0))) (NumV 1)

let test_interp_if_false () =
  check value "same int" (interp empty_env (If (Bool false, Num 1, Num 0))) (NumV 0)

let test_interp_compound_logic () =
  check value "same bool" (interp empty_env (If ((Lt (Num 1, Num 2)), (Eq (Num 1, Num 1)), Num 0)))
  (BoolV true)

let test_interp_fun () =
  check value "same closure" (interp empty_env (Fun ("x", Var "x")))
  (ClosureV ("x", (Var "x"), empty_env))

let test_interp_app () =
  check value "same int" (interp empty_env (App ((Fun ("x", Plus (Num 5, Var "x"))), Num 3)))
  (NumV 8)

let test_interp_compound_app () =
  check value "same int"
  (interp empty_env (App (Fun ("x", (App ((Fun ("y", (Plus (Var "y", Var "x")))), Num 2))), Num 1)))
  (NumV 3)

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
        test_case "A boolean" `Quick test_parse_bool ;
        test_case "A variable" `Quick test_parse_var ;
        test_case "A compound expression" `Quick test_parse_compound ;
        test_case "An anonymous fun" `Quick test_parse_fun ;
        test_case "An application" `Quick test_parse_app ;
        test_case "'let' syntax sugar" `Quick test_parse_let ;
        test_case "'let*' syntax sugar" `Quick test_parse_lets
      ] ;

      "interp", [
        test_case "A number" `Quick test_interp_num ;
        test_case "A variable" `Quick test_interp_var ;
        test_case "A boolean" `Quick test_interp_bool ;
        test_case "A function" `Quick test_interp_fun ;
        test_case "A compound expression" `Quick test_interp_compound ;
        test_case "An AND operation" `Quick test_interp_and ; 
        test_case "AND short-circuit" `Quick test_interp_and_shortcircuit ;
        test_case "If true" `Quick test_interp_if_true ;
        test_case "If false" `Quick test_interp_if_false ;
        test_case "A complex If statement" `Quick test_interp_compound_logic ;
        test_case "An application" `Quick test_interp_app ;
        test_case "An application compound" `Quick test_interp_compound_app
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