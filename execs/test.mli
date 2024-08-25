val exp : Dev.Ast.exp Alcotest.testable
val value : Dev.Lib.value Alcotest.testable
val test_parse_int : unit -> unit
val test_parse_var : unit -> unit
val test_parse_compound : unit -> unit
val test_parse_error : unit -> unit
val test_interp_num : unit -> unit
val test_interp_var : unit -> unit
val test_interp_compound : unit -> unit
val test_simplify_no_match : unit -> unit
val test_simplify_times_right_int : unit -> unit
val test_simplify_times_left_int : unit -> unit
val test_simplify_times_right_var : unit -> unit
val test_simplify_times_left_var : unit -> unit
val test_simplify_times_right_cmp : unit -> unit
val test_simplify_times_left_cmp : unit -> unit
val test_simplify_plus_right_int : unit -> unit
val test_simplify_plus_left_int : unit -> unit
val test_simplify_plus_right_var : unit -> unit
val test_simplify_plus_left_var : unit -> unit
val test_simplify_plus_right_cmp : unit -> unit
val test_simplify_plus_left_cmp : unit -> unit
