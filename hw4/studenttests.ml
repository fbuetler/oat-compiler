open Assert
open Gradedtests

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let own_tests = [
  ("atprograms/own/argc.oat", "test walrus", "3");
  ("atprograms/own/array_exp_order.oat", "", "3");
  ("atprograms/own/array.oat", "", "0");
  ("atprograms/own/bitnot.oat", "", "2");
  ("atprograms/own/bitwise.oat", "", "6");
  ("atprograms/own/bool.oat", "", "0");
  ("atprograms/own/bop.oat", "test abc", "9");
  ("atprograms/own/call_arg_order.oat", "", "3");
  ("atprograms/own/exp27.oat", "", "0");
  (* ("atprograms/own/grammar.oat", "", "0"); *)
  ("atprograms/own/if_exp_once.oat", "", "2");
  ("atprograms/own/lognot.oat", "", "1");
  ("atprograms/own/neg.oat", "", "3");
  (* ("atprograms/own/nested_global_arr.oat", "", "0"); TODO enable when question answered *)
  (* ("atprograms/own/not_call.oat", "", "0"); TODO enable when question answered *)
  ("atprograms/own/null_array.oat", "", "0");
  ("atprograms/own/null.oat", "", "0");
  ("atprograms/own/string.oat", "", "walrus0");
  ("atprograms/own/whilefalse.oat", "", "27");
]

let provided_tests : suite = [
  Test ("own tests", executed_oat_file own_tests);
] 
