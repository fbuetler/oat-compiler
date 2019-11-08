open Assert
open Gradedtests

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let uop_tests = [
  ("atprograms/own/neg.oat", "", "3");
  ("atprograms/own/lognot.oat", "", "1");
  ("atprograms/own/bitnot.oat", "", "2");
]

let provided_tests : suite = [
  Test ("uop tests", executed_oat_file uop_tests);
] 
