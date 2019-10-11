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

let student_instruction_tests = [
  make_instr_test "best test ever" 
    [(Movq, [~$42; ~%Rax]); (Movq, [~%Rax; stack_offset (-8L)])] 
    (fun m -> int64_of_sbytes (sbyte_list m.mem (mem_size-16)) = 42L)
  ;
]

let provided_tests : suite = [
  Test ("student_instruction_tests", student_instruction_tests);
  Test ("Student-Provided Big Test for Part III: Score recorded as PartIIITestCase", []);
] 
