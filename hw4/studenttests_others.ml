open Assert
open Gradedtests
open Ast
open Driver

let provided_tests : suite = [
  Test ("manual 1", executed_oat_file [("atprograms/others/bubble_sort.oat", "", "kpyf{shomfhkmopsy{255")]);
  Test ("manual 2", executed_oat_file [("atprograms/others/ackermann.oat", "", "125")]);
  Test ("manual 3", executed_oat_file [("atprograms/others/bucket_sort.oat", "", "42")]);
  Test ("manual 4", executed_oat_file [("atprograms/others/merge_sort.oat", "", "lkdfjkwegkljdfhljwegrnkljsvfw+.a+.addeefffgghjjjjkkkkllllnrsvwww255")]);
  
] 