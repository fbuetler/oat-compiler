open Assert
open Gradedtests

(* These tests are provided by you -- they will not be graded *)
(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let other_tests =
  [
  "ashu_ian_opt.ll", 5L
  ; "binary_gcd.ll", 3L
  ; "binarysearch.ll", 8L
  ; "call7.ll", 7L
  ; "call8.ll", 21L 
  ; "cbr3.ll", 9L
  ; "certified_random_number_generator.ll", 4L
  ; "certified_random_number_generator_soln.ll", 4L
  ; "euclid.ll", 2L
  ; "gcd_euclidian.ll", 2L
  ; "gep9.ll", 5L
  ; "kaiterry_pi.ll", 0L
  ; "kaiterry_pi_opt.ll", 0L
  ; "kaiterry_units.ll", 1L
  ; "kaiterry_units_opt.ll", 1L
  ; "kierajmumick.ll", 164L
  ; "kierajmumickopt.ll", 164L
  ; "lfsr.ll", 108L
  ; "linear_search.ll", 1L
  ; "matmul.ll", 0L
  ; "max_thomas_test.ll", 120L
  ; "max_thomas_test_opt.ll", 120L
  ; "naive_factor_nonprime.ll", 0L
  ; "naive_factor_prime.ll", 1L
  ; "opt_cbr_test1.ll", 0L
  ; "opt_globals_test1.ll", 1L
  ; "opt_globals_test1_soln.ll", 1L
  ; "qtree.ll", 3L
  ; "reed_nate_opt.ll", 196L
  ; "return_intermediate.ll", 18L
  ; "return_intermediate_dce.ll", 18L
  ; "return_intermediate_fold.ll", 18L
  ; "rsa.ll", 1L
  ; "rsaopt.ll", 1L
  ; "sum_tree.ll", 116L
  ; "vivekraj_jjlee.ll", 1L
  ; "vivekraj_jjlee_opt.ll", 1L
  ]

let append_ll_path = List.map(fun (x,y) -> "llprograms/" ^ x, y)

let add_tests = [ "add_twice.ll", 29L ]

let analysis_tests =
  [ "analysis1.ll", 49L
  ; "analysis10.ll", 60L
  ; "analysis11.ll", 3L
  ; "analysis11_cf_opt.ll",3L
  ; "analysis12.ll", 14L
  ; "analysis12_cf_opt.ll", 14L
  ; "analysis12_dce_opt.ll", 14L
  ; "analysis13.ll", 7L
  ; "analysis13_cf_opt.ll", 7L
  ; "analysis13_dce_opt.ll", 7L
  ; "analysis14.ll", 42L
  ; "analysis14_cf_opt.ll", 42L
  ; "analysis14_dce_opt.ll", 42L
  ; "analysis15.ll", 2L
  ; "analysis15_cf_opt.ll", 2L
  ; "analysis15_dce_opt.ll", 2L
  ; "analysis16.ll", 1L
  ; "analysis18.ll", 42L
  ; "analysis18_cf_opt.ll", 42L
  ; "analysis18_dce_opt.ll", 42L
  ; "analysis19.ll", 5L
  ; "analysis19_cf_opt.ll", 5L
  ; "analysis19_dce_opt.ll", 5L
  ; "analysis1_cf_opt.ll", 49L
  ; "analysis2.ll", 8L
  ; "analysis2_cf_opt.ll", 8L
  ; "analysis3.ll", 188L
  ; "analysis3_cf_opt.ll", 188L
  ; "analysis3_dce_opt.ll", 188L
  ; "analysis4.ll",254L
  ; "analysis4_cf_opt.ll", 254L
  ; "analysis5.ll", 14L
  ; "analysis5_cf_opt.ll", 14L
  ; "analysis5_dce_opt.ll", 14L
  ; "analysis6_cf_opt.ll",  2L
  ; "analysis6_dce_opt.ll", 2L
  ; "analysis7.ll", 10L
  ; "analysis8.ll", 95L
  ; "analysis8_cf_opt.ll", 95L
  ; "analysis8_dce_opt.ll", 95L
  ; "analysis9.ll", 0L
]

let arith_combo = [
    "arith_combo.ll", 4L
  ; "arith_combo_dce.ll", 4L
  ; "arith_combo_fold.ll", 4L
]

(* negative values don't seem to work with execution environment *)
let sub_tests =
  [ "sub_neg.ll", -1L
  ; "sub_neg_dce.ll", -1L
  ; "sub_neg_fold.ll", -1L
  ]

let provided_tests : suite = [
  Test("add_tests", append_ll_path add_tests |> executed) ;

  Test("analysis_tests", append_ll_path analysis_tests |> executed) ; 

  Test("other_tests", append_ll_path other_tests |> executed)
] 
