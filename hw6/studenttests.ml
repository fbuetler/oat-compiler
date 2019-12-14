open Assert
open Datastructures

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let provided_tests : suite = [
  Test("UidGraph.empty", [
      "has no nodes", assert_eq (UidS.is_empty @@ UidGraph.get_nodes UidGraph.empty) true;
    ]);
] 
