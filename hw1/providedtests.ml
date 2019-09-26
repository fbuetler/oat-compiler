open Assert
open Hellocaml

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let provided_tests : suite = [
  Test ("Student-Provided Tests For Problem 1-3", [
    ("case1", assert_eqf (fun () -> 42) prob3_ans );
    ("case2", assert_eqf (fun () -> 25) (prob3_case2 17) );
    ("case3", assert_eqf (fun () -> prob3_case3) 64);
  ]);
  
  Test ("Student-Provided Tests For Problem 4-5", [
    ("optimize1", assert_eqf (fun () -> optimize (Add(Const 3L, Neg (Const 0L)))) (Const 3L));
  ]);

  (*
    (interpret c e) = v       if and only if
    (run c (compile e)) = v
  *)

  Test ("Student-Provided Tests For Problem 5", [
    ("compile1", assert_eqf (fun () -> compile (Mult(Const 2L, Const 3L))) ([IPushC 2L; IPushC 3L; IMul]));
  ]);

  Test ("Student-Provided Correctness Tests For Problem 5", [
    let exp = (Add(Mult(Var "x", Var "y"), Const 5L)) in
      let context = [("x", 2L); ("y", 7L)] in
      ("correctness1", assert_eqf (fun () -> (interpret context exp)) (run context (compile exp)));
    let exp = (Add(Var "x", Const 1L)) in
      let context = [("x", 3L)]  in
      ("correctness2", assert_eqf (fun () -> (interpret context exp)) (run context (compile exp)));
  ]);

] 
