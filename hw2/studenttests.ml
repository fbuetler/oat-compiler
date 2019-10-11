open Assert
open X86
open Simulator
open Gradedtests
open Asm

let opcode_to_string (op:opcode) : string =
  begin match op with
    | Addq -> "Addq"
    | Subq -> "Subq"
    | Imulq -> "Imulq"
    | Incq -> "Incq"
    | Decq -> "Decq"
    | Negq -> "Negq"
    | Andq -> "Andq"
    | Orq -> "Orq"
    | Xorq -> "Xorq"
    | Notq -> "Notq"
    | Sarq -> "Sarq"
    | Shlq -> "Shlq"
    | Shrq -> "Shrq"
    | Set _ -> "cc"
    | Leaq -> "Leaq"
    | Movq -> "Movq"
    | Pushq -> "Pushq"
    | Popq -> "Popq"
    | Cmpq -> "Cmpq"
    | Callq -> "Callq"
    | Retq -> "Retq"
    | Jmp -> "Jmp"
    | J _ -> "cc"
  end

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

let make_bin_op_test (op:opcode) (a:int64) (b:int64) (res:int64) (flags:flags) =
  let label = Printf.sprintf "%s %s %s = %s"
      (opcode_to_string op) (Int64.to_string a) (Int64.to_string b) (Int64.to_string res) in
  let get_res (m:mach) = int64_of_sbytes (sbyte_list m.mem (mem_size-8)) in
  make_instr_test_print label
    [(Movq, [Imm (Lit b); ~%Rax])
    ;(op, [Imm (Lit a); ~%Rax])
    ;(Movq, [~%Rax; stack_offset 0L])] 
    (fun m -> get_res m = res && m.flags = flags)
    (fun m -> Printf.sprintf "res = %s, flags = {fo=%B, fs=%B, fz=%B}"
        (Int64.to_string @@ get_res m) m.flags.fo m.flags.fs m.flags.fz)

let make_un_op_test (op:opcode) (a:int64) (res:int64) (flags:flags) =
  let label = Printf.sprintf "%s %s = %s"
      (opcode_to_string op) (Int64.to_string a) (Int64.to_string res) in
  let get_res (m:mach) = int64_of_sbytes (sbyte_list m.mem (mem_size-8)) in
  make_instr_test_print label
    [(Movq, [Imm (Lit a); ~%Rax])
    ;(op, [~%Rax])
    ;(Movq, [~%Rax; stack_offset 0L])] 
    (fun m -> get_res m = res && m.flags = flags)
    (fun m -> Printf.sprintf "res = %s, flags = {fo=%B, fs=%B, fz=%B}"
        (Int64.to_string @@ get_res m) m.flags.fo m.flags.fs m.flags.fz)

let student_instruction_tests_flo = [
  make_bin_op_test Addq 123123L 42424242L 42547365L {fo=false; fs=false; fz=false}; (* random add *)
  make_bin_op_test Addq 1L 9223372036854775807L 9223372036854775808L {fo=true; fs=true; fz=false}; (* overflow add: T_Max + 1 = |T_Min| *)
  make_bin_op_test Imulq 8L 4L 32L {fo=false; fs=false; fz=false};
  make_bin_op_test Imulq 4L 8L 32L {fo=false; fs=false; fz=false};
  make_bin_op_test Imulq 8L 0L 0L {fo=false; fs=false; fz=true};
  make_bin_op_test Imulq 8L 1L 8L {fo=false; fs=false; fz=false};
  make_bin_op_test Imulq 3037000500L 3037000500L (-9223372036709301616L) {fo=true; fs=true; fz=false};
  make_un_op_test Decq 5L 4L {fo=false; fs=false; fz=false};
  make_un_op_test Decq 1L 0L {fo=false; fs=false; fz=true};
  make_un_op_test Decq 0L (-1L) {fo=false; fs=true; fz=false};
  make_un_op_test Decq (-2L) (-3L) {fo=false; fs=true; fz=false};
  make_bin_op_test Andq 0b1001L 0b1100L 0b1000L {fo=false; fs=false; fz=false};
  make_bin_op_test Andq 0b1111L 0b0000L 0b0000L {fo=false; fs=false; fz=true};
  make_bin_op_test Xorq 0b1111L 0b0001L 0b1110L {fo=false; fs=false; fz=false};
  make_bin_op_test Xorq 0b101000111011L 
                        0b011101011001L 
                        0b110101100010L {fo=false; fs=false; fz=false};
  make_bin_op_test Xorq 0b1010L 0b1010L 0b0000L {fo=false; fs=false; fz=true};                      
  make_bin_op_test Sarq 1L 4L 2L {fo=false; fs=false; fz=false}; (* small random arithmetic shift *)
  make_bin_op_test Sarq 13L 1588476L 193L {fo=false; fs=false; fz=false}; (* big random arithmetic shift *)
  make_bin_op_test Sarq 10L (-1L) (-1L) {fo=false; fs=true; fz=false}; (* arithmetic shifting only ones shouldnt change anything*)
  make_bin_op_test Sarq 3L 0L 0L {fo=false; fs=false; fz=true}; (* arithmetic shifting 0 shouldnt change anything *)
  make_bin_op_test Sarq 3L 8L 1L {fo=false; fs=false; fz=false}; (* arithmetic sfhit: MSB is 1 *)
  make_bin_op_test Shrq 4L 8L 0L {fo=false; fs=false; fz=true}; (* logical shift *)
  make_instr_test "leaq1" 
    [(Leaq, [Ind1 (Lit 42L); ~%Rax])] 
    (fun m -> m.regs.(rind Rax) = 42L)
  ;
  make_instr_test "leaq2" 
    [(Movq, [~$42; ~%Rcx])
    ;(Leaq, [Ind2 (Rcx); ~%Rax])] 
    (fun m -> m.regs.(rind Rax) = 42L)
  ;
  make_instr_test "leaq3" 
    [(Movq, [~$42; ~%Rcx])
    ;(Leaq, [Ind3 (Lit 3L, Rcx); ~%Rax])] 
    (fun m -> m.regs.(rind Rax) = 45L)
  ;
  make_instr_test "pushq1" 
    [(Pushq, [~$42])] 
    (fun m -> int64_of_sbytes (sbyte_list m.mem (mem_size-16)) = 42L
              && m.regs.(rind Rsp) = (Int64.sub mem_top 16L)
    )
  ;
  make_instr_test "pushq2" 
    [(Pushq, [~$42])
    ;(Pushq, [~$27])
    ] 
    (fun m -> int64_of_sbytes (sbyte_list m.mem (mem_size-16)) = 42L
              && int64_of_sbytes (sbyte_list m.mem (mem_size-24)) = 27L
              && m.regs.(rind Rsp) = (Int64.sub mem_top 24L)
    )
  ;
  make_instr_test "pushq popq" 
    [(Pushq, [~$42])
    ;(Popq, [~%Rax])] 
    (fun m -> m.regs.(rind Rax) = 42L
              && m.regs.(rind Rsp) = (Int64.sub mem_top 8L)
    )
  ;
  make_instr_test "pushq pushq popq popq" 
    [(Pushq, [~$42])
    ;(Pushq, [~$3])
    ;(Popq, [~%Rcx])
    ;(Popq, [~%Rax])] 
    (fun m -> m.regs.(rind Rax) = 42L
              && m.regs.(rind Rcx) = 3L
              && m.regs.(rind Rsp) = (Int64.sub mem_top 8L)
    )
  ;
  make_instr_test "cmpq1" 
    [(Cmpq, [~$42; ~$7])] 
    (fun m -> m.flags.fo = false
              && m.flags.fs = true
              && m.flags.fz = false)
  ;
  make_instr_test "cmpq2" 
    [(Cmpq, [~$7; ~$42])] 
    (fun m -> m.flags.fo = false
              && m.flags.fs = false
              && m.flags.fz = false)
  ;
  make_instr_test "cmpq3" 
    [(Cmpq, [~$42; ~$42])] 
    (fun m -> m.flags.fo = false
              && m.flags.fs = false
              && m.flags.fz = true)
  ;
  make_instr_test "cmpq4" 
    [(Cmpq, [~$1; Imm (Lit 9223372036854775808L)])] 
    (fun m -> m.flags.fo = true
              && m.flags.fs = false
              && m.flags.fz = false)
  ;
  make_instr_test "cmpq5" 
    [(Cmpq, [~$(-1); Imm (Lit 9223372036854775807L)])] 
    (fun m -> m.flags.fo = true
              && m.flags.fs = true
              && m.flags.fz = false)
  ;
  make_instr_test "j Eq 1" 
    [(Cmpq, [~$42; ~$42])
    ;(J Eq, [~$0x400100])
    ] 
    (fun m -> m.regs.(rind Rip) = 0x400100L)
  ;
  make_instr_test "j Eq 2" 
    [(Cmpq, [~$42; ~$3])
    ;(J Eq, [~$0x400100])
    ] 
    (fun m -> m.regs.(rind Rip) = (Int64.add mem_bot 16L))
  ;
  make_instr_test "j Neq 1" 
    [(Cmpq, [~$42; ~$42])
    ;(J Neq, [~$0x400100])
    ] 
    (fun m -> m.regs.(rind Rip) = (Int64.add mem_bot 16L))
  ;
  make_instr_test "j Neq 2" 
    [(Cmpq, [~$42; ~$3])
    ;(J Neq, [~$0x400100])
    ] 
    (fun m -> m.regs.(rind Rip) = 0x400100L)
  ;
  make_instr_test "j Gt 1" 
    [(Cmpq, [~$3; ~$42])
    ;(J Gt, [~$0x400100])
    ] 
    (fun m -> m.regs.(rind Rip) = 0x400100L)
  ;
  make_instr_test "j Gt 2" 
    [(Cmpq, [~$42; ~$3])
    ;(J Gt, [~$0x400100])
    ] 
    (fun m -> m.regs.(rind Rip) = (Int64.add mem_bot 16L))
  ;
  make_instr_test "j Ge 1" 
    [(Cmpq, [~$42; ~$42])
    ;(J Ge, [~$0x400100])
    ] 
    (fun m -> m.regs.(rind Rip) = 0x400100L)
  ;
  make_instr_test "j Ge 2" 
    [(Cmpq, [~$41; ~$42])
    ;(J Ge, [~$0x400100])
    ] 
    (fun m -> m.regs.(rind Rip) = 0x400100L)
  ;
  make_instr_test "j Ge 3" 
    [(Cmpq, [~$43; ~$42])
    ;(J Ge, [~$0x400100])
    ] 
    (fun m -> m.regs.(rind Rip) = (Int64.add mem_bot 16L))
  ;
  make_instr_test "j Lt 1" 
    [(Cmpq, [~$42; ~$3])
    ;(J Lt, [~$0x400100])
    ] 
    (fun m -> m.regs.(rind Rip) = 0x400100L)
  ;
  make_instr_test "j Lt 2" 
    [(Cmpq, [~$3; ~$42])
    ;(J Lt, [~$0x400100])
    ] 
    (fun m -> m.regs.(rind Rip) = (Int64.add mem_bot 16L))
  ;
  make_instr_test "j Le 1" 
    [(Cmpq, [~$42; ~$42])
    ;(J Le, [~$0x400100])
    ] 
    (fun m -> m.regs.(rind Rip) = 0x400100L)
  ;
  make_instr_test "j Le 2" 
    [(Cmpq, [~$42; ~$41])
    ;(J Le, [~$0x400100])
    ] 
    (fun m -> m.regs.(rind Rip) = 0x400100L)
  ;
  make_instr_test "j Le 3" 
    [(Cmpq, [~$42; ~$43])
    ;(J Le, [~$0x400100])
    ] 
    (fun m -> m.regs.(rind Rip) = (Int64.add mem_bot 16L))
  ;

]

(* TODO merge this with the other tests *)
let student_instruction_tests_philippe = [
  make_bin_op_test Subq 0L 0L 0L {fo=false; fs=false; fz=true};
  make_bin_op_test Subq 3L (-7L) (-10L) {fo=false; fs=true; fz=false};
  make_bin_op_test Subq (-7L) 3L 10L {fo=false; fs=false; fz=false};
  make_bin_op_test Subq (-7L) (-7L) 0L {fo=false; fs=false; fz=true};
  make_bin_op_test Subq 1L Int64.min_int Int64.max_int {fo=true; fs=false; fz=false};
  make_bin_op_test Subq Int64.max_int 0L (Int64.neg Int64.max_int) {fo=false; fs=true; fz=false};
  make_bin_op_test Subq Int64.min_int 0L Int64.min_int {fo=true; fs=true; fz=false};
  make_un_op_test Incq 0L 1L {fo=false; fs=false; fz=false};
  make_un_op_test Incq 5L 6L {fo=false; fs=false; fz=false};
  make_un_op_test Incq (-1L) 0L {fo=false; fs=false; fz=true};
  make_un_op_test Incq (-2L) (-1L) {fo=false; fs=true; fz=false};
  make_un_op_test Incq Int64.max_int Int64.min_int {fo=true; fs=true; fz=false};
  make_un_op_test Negq 0L 0L {fo=false; fs=false; fz=true};
  make_un_op_test Negq 1L (-1L) {fo=false; fs=true; fz=false};
  make_un_op_test Negq (-1L) 1L {fo=false; fs=false; fz=false};
  make_un_op_test Negq 5L (-5L) {fo=false; fs=true; fz=false};
  make_un_op_test Negq Int64.max_int (Int64.neg Int64.max_int) {fo=false; fs=true; fz=false};
  make_bin_op_test Orq 0L 0L 0L {fo=false; fs=false; fz=true};
  make_bin_op_test Orq 7L 7L 7L {fo=false; fs=false; fz=false};
  make_bin_op_test Orq (-1L) (-1L) (-1L) {fo=false; fs=true; fz=false};
  make_bin_op_test Orq 0b01L 0b10L 0b11L {fo=false; fs=false; fz=false};
  make_bin_op_test Orq
    0b0111011000111110101110110010110110101110101010111011110111011110L
    0b0101001101101000101011101010111110101010001101000101010110110100L
    0b0111011101111110101111111010111110101110101111111111110111111110L
    {fo=false; fs=false; fz=false};
  make_un_op_test Notq 0L (-1L) {fo=false; fs=false; fz=false};
  make_un_op_test Notq (-1L) 0L {fo=false; fs=false; fz=false};
  make_un_op_test Notq
    6L
    0b1111111111111111111111111111111111111111111111111111111111111001L
    {fo=false; fs=false; fz=false};
  make_un_op_test Notq
    0b0111011000111110101110110010110110101110101010111011110111011110L
    0b1000100111000001010001001101001001010001010101000100001000100001L
    {fo=false; fs=false; fz=false};
]

let provided_tests : suite = [
  Test ("student_instruction_tests_flo", student_instruction_tests_flo);
  Test ("student_instruction_tests_philippe", student_instruction_tests_philippe);
  Test ("Student-Provided Big Test for Part III: Score recorded as PartIIITestCase", []);
] 
