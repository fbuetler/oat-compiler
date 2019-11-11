open Assert
open Gradedtests
(*
17-913-534, Florian Bütler
16-926-560, Philippe Voinov
*)

let own_tests = [
  ("atprograms/own/base64.oat", "walrus", "d2FscnVz0");
  ("atprograms/own/base64.oat", "florian", "Zmxvcmlhbg==0");
  ("atprograms/own/base64.oat", "philippe", "cGhpbGlwcGU=0");
  ("atprograms/own/base64.oat", "CompilerDesign", "Q29tcGlsZXJEZXNpZ24=0");
  ("atprograms/own/base64.oat", "VIS", "VklT0");
  ("atprograms/own/base64.oat", "VereinDerInformatikStudierenden", "VmVyZWluRGVySW5mb3JtYXRpa1N0dWRpZXJlbmRlbg==0");
]

let provided_tests : suite = [
  Test ("own tests", executed_oat_file own_tests);
] 
