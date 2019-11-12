open Assert
open Gradedtests

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let own_tests = [
  ("atprograms/own/argc.oat", "test walrus", "3");
  ("atprograms/own/array_exp_order.oat", "", "3");
  ("atprograms/own/array.oat", "", "0");
  ("atprograms/own/assign_index_return.oat", "", "5");
  ("atprograms/own/bitnot.oat", "", "2");
  ("atprograms/own/bitwise.oat", "", "6");
  ("atprograms/own/bool.oat", "", "0");
  ("atprograms/own/bop.oat", "test abc", "9");
  ("atprograms/own/call_arg_order.oat", "", "3");
  ("atprograms/own/exp27.oat", "", "0");
  ("atprograms/own/if_exp_once.oat", "", "2");
  ("atprograms/own/lognot.oat", "", "1");
  ("atprograms/own/neg.oat", "", "3");
  ("atprograms/own/null_array.oat", "", "0");
  ("atprograms/own/null.oat", "", "0");
  ("atprograms/own/string.oat", "", "walrus0");
  ("atprograms/own/whilefalse.oat", "", "27");
  ("atprograms/own/global_string_array.oat", "", "String20");
  ("atprograms/own/multidim.oat", "", "2");
  ("studenttests/base64.oat", "walrus", "d2FscnVz0");
  ("studenttests/base64.oat", "florian", "Zmxvcmlhbg==0");
  ("studenttests/base64.oat", "philippe", "cGhpbGlwcGU=0");
  ("studenttests/base64.oat", "CompilerDesign", "Q29tcGlsZXJEZXNpZ24=0");
  ("studenttests/base64.oat", "VIS", "VklT0");
  ("studenttests/base64.oat", "VereinDerInformatikStudierenden", "VmVyZWluRGVySW5mb3JtYXRpa1N0dWRpZXJlbmRlbg==0");
]

let provided_tests : suite = [
  Test ("own tests", executed_oat_file own_tests);
] 
