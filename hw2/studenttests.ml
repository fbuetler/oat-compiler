open Assert
open X86
open Simulator
open Gradedtests
open Asm

let make_instr_test (label:string) (instructions:ins list) (check: mach -> bool) =
  let pad_instr (instr:ins) = [InsB0 (instr);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag] in
  let m = test_machine @@ List.flatten @@ List.map pad_instr instructions in
  let t = machine_test "<see check function>" (List.length instructions) m check in
  (label, t)

let make_instr_test_print (label:string) (instructions:ins list) (check: mach -> bool) (print: mach -> string) =
  let pad_instr (instr:ins) = [InsB0 (instr);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag] in
  let m = test_machine @@ List.flatten @@ List.map pad_instr instructions in
  let base_test = machine_test "<see check function>" (List.length instructions) m check in
  let wrapped_test = fun () ->
    try base_test ()
    with _ -> failwith @@ print m
  in (label, wrapped_test)

let make_subq_test (a:int64) (b:int64) (res:int64) (flags:flags) =
  let label = Printf.sprintf "subq (%s - %s) = %s"
      (Int64.to_string a) (Int64.to_string b) (Int64.to_string res) in
  let get_res (m:mach) = int64_of_sbytes (sbyte_list m.mem (mem_size-16)) in
  make_instr_test_print label
    [(Movq, [Imm (Lit a); ~%Rax])
    ;(Movq, [Imm (Lit b); ~%Rax])] 
    (fun m -> get_res m = res && m.flags = flags)
    (fun m -> Printf.sprintf "res = %s, flags = {fo: %B, fs: %B, fz: %B}"
        (Int64.to_string @@ get_res m) m.flags.fo m.flags.fs m.flags.fz)

let student_instruction_tests = [
  make_instr_test "best test ever" 
    [(Movq, [~$42; ~%Rax]); (Movq, [~%Rax; stack_offset (-8L)])] 
    (fun m -> int64_of_sbytes (sbyte_list m.mem (mem_size-16)) = 42L)
  ;
]

(* TODO merge this with the other tests *)
let student_instruction_tests_philippe = [
  make_subq_test 0L 0L 1L {fo=false; fs=false; fz=false}
]

let provided_tests : suite = [
  Test ("student_instruction_tests", student_instruction_tests);
  Test ("student_instruction_tests_philippe", student_instruction_tests_philippe);
  Test ("Student-Provided Big Test for Part III: Score recorded as PartIIITestCase", []);
] 
