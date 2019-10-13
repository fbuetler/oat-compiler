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
  make_instr_test "callq" 
    [(Callq, [~$0x400100])]
    (fun m -> m.regs.(rind Rip) = 0x400100L
              && m.regs.(rind Rsp) = (Int64.sub mem_top 16L)
              && int64_of_sbytes (sbyte_list m.mem (mem_size-16)) = Int64.add mem_bot 8L
    )
  ;
  make_instr_test "retq" 
    [(Pushq, [~$0x400100])
    ;(Retq, [])] 
    (fun m -> m.regs.(rind Rip) = 0x400100L
              && m.regs.(rind Rsp) = (Int64.sub mem_top 8L))
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

  (* load tests *)
  let helloworld_dataseg =
    [ Byte 'c'; Byte '\x00'; Byte '\x00'; Byte '\x00'
    ; Byte '\x00'; Byte '\x00'; Byte '\x00'; Byte '\x00'
    ; Byte 'H'; Byte 'e' ; Byte 'l'; Byte 'l'
    ; Byte 'o'; Byte ','; Byte ' '; Byte 'w'
    ; Byte 'o'; Byte 'r'; Byte 'l'; Byte 'd'
    ; Byte '!'; Byte '\x00' ] in
  let helloworld_textseg =
    [ InsB0 (Xorq, [Reg Rax; Reg Rax]);              InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ; InsB0 (Movq, [Imm (Lit 100L); Reg Rax]);       InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ; InsB0 (Retq, []);                              InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ; InsB0 (Xorq, [Reg Rax; Reg Rax]);              InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ; InsB0 (Movq, [Ind1 (Lit 0x400030L); Reg Rax]); InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ; InsB0 (Retq, []);                              InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ] in
  let test_exec: exec =
    { entry = 0x400008L
    ; text_pos = 0x400000L
    ; data_pos = 0x400064L
    ; text_seg = helloworld_textseg
    ; data_seg = helloworld_dataseg
    } in 
  ("load_helloworld", assert_eqf (fun () -> (load test_exec).regs.(rind Rip)) 0x400008L);
  (* TODO: test the rest of load *)
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


(* ##### start: tests jan ##### *)
let inss_to_sbytes (inss:ins list) : sbyte list =
  List.map sbytes_of_ins inss |> List.flatten

let machine_test_inss (inss:ins list) =
  inss_to_sbytes inss |> test_machine |> machine_test "" (List.length inss)

type cc_expected = CC_set | CC_cleared | CC_unchanged

let machine_test_cc (inss:ins list) (fo', fs', fz') () : unit =
  List.iter (fun init ->
      let expect : cc_expected -> bool = function
        | CC_set -> true
        | CC_cleared -> false
        | CC_unchanged -> init in
      let m = inss_to_sbytes inss |> test_machine in
      machine_test "" (List.length inss)
        {m with flags = {fo = init; fs = init; fz = init}}
        (fun {flags} ->
           expect fo' = flags.fo &&
           expect fs' = flags.fs &&
           expect fz' = flags.fz) ()
    ) [false; true]


let student_instruction_tests_jan = [
  (* unary ops *)
  ("negq", machine_test_inss
     [Movq, [~$42; ~%Rax]; Negq, [~%Rax]]
     (fun m -> m.regs.(rind Rax) = -42L)
  );
  ("incq", machine_test_inss
     [Movq, [~$42; ~%Rax]; Incq, [~%Rax]]
     (fun m -> m.regs.(rind Rax) = 43L)
  );
  ("decq", machine_test_inss
     [Movq, [~$42; ~%Rax]; Decq, [~%Rax]]
     (fun m -> m.regs.(rind Rax) = 41L)
  );
  ("notq", machine_test_inss
     [Movq, [~$42; ~%Rax]; Notq, [~%Rax]]
     (fun m -> m.regs.(rind Rax) = -43L)
  );
  (* binary ops *)
  ("addq", machine_test_inss
     [Movq, [~$42; ~%Rax]; Movq, [~$13; ~%Rbx]; Addq, [~%Rbx; ~%Rax]]
     (fun m -> m.regs.(rind Rax) = 55L)
  );
  ("subq", machine_test_inss
     [Movq, [~$42; ~%Rax]; Movq, [~$13; ~%Rbx]; Subq, [~%Rbx; ~%Rax]]
     (fun m -> m.regs.(rind Rax) = 29L)
  );
  ("imulq", machine_test_inss
     [Movq, [~$42; ~%Rax]; Movq, [~$(-13); ~%Rbx]; Imulq, [~%Rbx; ~%Rax]]
     (fun m -> m.regs.(rind Rax) = -546L)
  );
  ("xorq", machine_test_inss
     [Movq, [~$42; ~%Rax]; Movq, [~$13; ~%Rbx]; Xorq, [~%Rbx; ~%Rax]]
     (fun m -> m.regs.(rind Rax) = 39L)
  );
  ("orq", machine_test_inss
     [Movq, [~$42; ~%Rax]; Movq, [~$13; ~%Rbx]; Orq, [~%Rbx; ~%Rax]]
     (fun m -> m.regs.(rind Rax) = 47L)
  );
  ("andq", machine_test_inss
     [Movq, [~$42; ~%Rax]; Movq, [~$13; ~%Rbx]; Andq, [~%Rbx; ~%Rax]]
     (fun m -> m.regs.(rind Rax) = 8L)
  );
  ("sarq", machine_test_inss
     [Movq, [~$(-42); ~%Rax]; Movq, [~$2; ~%Rcx]; Sarq, [~%Rcx; ~%Rax]]
     (fun m -> m.regs.(rind Rax) = -11L)
  );
  ("shlq", machine_test_inss
     [Movq, [~$(-42); ~%Rax]; Movq, [~$2; ~%Rcx]; Shlq, [~%Rcx; ~%Rax]]
     (fun m -> m.regs.(rind Rax) = -168L)
  );
  ("shrq", machine_test_inss
     [Movq, [~$(-42); ~%Rax]; Movq, [~$2; ~%Rcx]; Shrq, [~%Rcx; ~%Rax]]
     (fun m -> m.regs.(rind Rax) = 0x3ffffffffffffff5L)
  );
  (* other ops *)
  ("leaq", machine_test_inss
     [Movq, [~$42; ~%Rax]; Leaq, [Ind3 (Lit 13L, Rax); ~%Rbx]]
     (fun m -> m.regs.(rind Rbx) = 55L)
  );
  ("pushq", machine_test_inss
     [Movq, [~$42; ~%Rax]; Pushq, [~%Rax]]
     (fun m -> m.regs.(rind Rax) = 42L
               && m.regs.(rind Rsp) = Int64.sub mem_top 16L
               && int64_of_sbytes (sbyte_list m.mem (mem_size-16)) = 42L)
  );
  ("popq", machine_test_inss
     [Movq, [~$42; Ind2 Rsp]; Popq, [~%Rax]]
     (fun m -> m.regs.(rind Rax) = 42L
               && m.regs.(rind Rsp) = mem_top
               && int64_of_sbytes (sbyte_list m.mem (mem_size-8)) = 42L)
  );
  ("set true", machine_test_inss
     [Movq, [Imm (Lit 0x123456789abcdefL); ~%Rax]; Cmpq, [~$1; ~$2]; Set Gt, [~%Rax]]
     (fun m -> m.regs.(rind Rax) = 0x123456789abcd01L)
  );
  ("set false", machine_test_inss
     [Movq, [Imm (Lit 0x123456789abcdefL); ~%Rax]; Cmpq, [~$2; ~$2]; Set Gt, [~%Rax]]
     (fun m -> m.regs.(rind Rax) = 0x123456789abcd00L)
  );
  ("set mem", machine_test_inss
     [Movq, [Imm (Lit 0x123456789abcdefL); Ind2 Rsp]; Cmpq, [~$1; ~$2]; Set Gt, [Ind2 Rsp]]
     (fun m -> int64_of_sbytes (sbyte_list m.mem (mem_size-8)) = 0x123456789abcd01L)
  );
  ("set at mem top does not cause segfault", machine_test_inss
     [Addq, [~$7; ~%Rsp]; Cmpq, [~$1; ~$2]; Set Gt, [Ind2 Rsp]]
     (fun m -> (m.mem.(mem_size-1)) = Byte (Char.chr 1))
  );
  ("jmp", machine_test_inss
     [Jmp, [~$42]]
     (fun m -> m.regs.(rind Rip) = 42L)
  );
  ("j true", machine_test_inss
     [Cmpq, [~$1; ~$2]; J Gt, [~$42]]
     (fun m -> m.regs.(rind Rip) = 42L)
  );
  ("j false", machine_test_inss
     [Cmpq, [~$2; ~$2]; J Gt, [~$42]]
     (fun m -> m.regs.(rind Rip) = Int64.add mem_bot 16L)
  );
  ("retq", machine_test_inss
     [Movq, [~$42; Ind2 Rsp]; Retq, []]
     (fun m -> m.regs.(rind Rip) = 42L
               && m.regs.(rind Rsp) = mem_top)
  );
  ("callq", machine_test_inss
     [Callq, [~$42]]
     (fun m -> m.regs.(rind Rip) = 42L
               && m.regs.(rind Rsp) = Int64.sub mem_top 16L
               && int64_of_sbytes (sbyte_list m.mem (mem_size-16)) =
                  Int64.add mem_bot 8L)
  );
  ("notq should not touch flags", machine_test_cc
     [Movq, [~$42; ~%Rax]; Notq, [~%Rax]]
     (CC_unchanged, CC_unchanged, CC_unchanged)
  );
  ("imulq no overflow", machine_test_inss
     [Movq, [~$42; ~%Rax]; Movq, [~$(-13); ~%Rbx]; Imulq, [~%Rbx; ~%Rax]]
     (fun m -> m.flags.fo = false)
  );
  ("imulq overflow", machine_test_inss
     [Movq, [Imm (Lit 0x0001000000000000L); ~%Rax];  Imulq, [~%Rax; ~%Rax]]
     (fun m -> m.flags.fo = true)
  );
  (* if AMT=0 flags are unaffected *)
  ("sarq-flags-amt0", machine_test_cc
     [Movq, [~$(-42); ~%Rax]; Sarq, [~$0; ~%Rax]]
     (CC_unchanged, CC_unchanged, CC_unchanged)
  );
  ("shlq-flags-amt0", machine_test_cc
     [Movq, [~$(-42); ~%Rax]; Shlq, [~$0; ~%Rax]]
     (CC_unchanged, CC_unchanged, CC_unchanged)
  );
  ("shrq-flags-amt0", machine_test_cc
     [Movq, [~$(-42); ~%Rax]; Shrq, [~$0; ~%Rax]]
     (CC_unchanged, CC_unchanged, CC_unchanged)
  );
  (* if AMT=1 then fo=0, fs and fz normal*)
  ("sarq-flags-amt1", machine_test_cc
     [Movq, [~$(-42); ~%Rax]; Movq, [~$1; ~%Rcx]; Sarq, [~%Rcx; ~%Rax]]
     (CC_cleared, CC_set, CC_cleared)
  );
  (* OF is set if the top two bits of DEST are different and the shift amount is 1 *)
  ("shlq-flags-amt1-01", machine_test_cc
     [Movq, [Imm (Lit 0x4000000000000000L); ~%Rax]; Shlq, [~$1; ~%Rax]]
     (CC_set, CC_set, CC_cleared)
  );
  ("shlq-flags-amt1-10", machine_test_cc
     [Movq, [Imm (Lit 0x8000000000000000L); ~%Rax]; Shlq, [~$1; ~%Rax]]
     (CC_set, CC_cleared, CC_set)
  );
  ("shlq-flags-amt1-00", machine_test_cc
     [Movq, [~$0; ~%Rax]; Shlq, [~$1; ~%Rax]]
     (CC_unchanged, CC_cleared, CC_set)
  );
  ("shlq-flags-amt1-11", machine_test_cc
     [Movq, [Imm (Lit 0xc000000000000000L); ~%Rax]; Shlq, [~$1; ~%Rax]]
     (CC_unchanged, CC_set, CC_cleared)
  );
  (* OF is set to the most-significant bit of the original operand if the shift amount is 1 *)
  ("shrq-flags-amt1", machine_test_cc
     [Movq, [~$1; ~%Rax]; Shrq, [~$1; ~%Rax]]
     (CC_cleared, CC_cleared, CC_set)
  );
  ("shrq-flags-amt1", machine_test_cc
     [Movq, [~$(-1); ~%Rax]; Shrq, [~$1; ~%Rax]]
     (CC_set, CC_cleared, CC_cleared)
  );
  (* if AMT<>1 then fo is unaffected *)
  ("sarq-flags-amt3", machine_test_cc
     [Movq, [~$(-42); ~%Rax]; Sarq, [~$3; ~%Rax]]
     (CC_unchanged, CC_set, CC_cleared)
  );
  ("shlq-flags-amt3", machine_test_cc
     [Movq, [~$(-42); ~%Rax]; Shlq, [~$3; ~%Rax]]
     (CC_unchanged, CC_set, CC_cleared)
  );
  ("shrq-flags-amt3", machine_test_cc
     [Movq, [~$(-42); ~%Rax]; Shrq, [~$3; ~%Rax]]
     (CC_unchanged, CC_cleared, CC_cleared)
  );
  ("fact6-iter", program_test (factorial_iter 6) 720L);
]
(* ##### end: tests jan ##### *)

(* ##### start: tests christian ##### *)
let cc_from_to (n:int) (m:mach) (fo',fs',fz') (fo'',fs'',fz'') = 
  cc_test (Printf.sprintf "expected OF:%b SF:%b ZF:%b" fo'' fs'' fz'')
    n m (fo',fs',fz')
    (fun m -> m.flags.fo = fo'' && m.flags.fs = fs'' && m.flags.fz = fz'')


(* Additional shift cc tests*)
let cc_sarq_0 = test_machine  
    [InsB0 (Movq, [~$0x400600; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Sarq, [(Imm (Lit 0L)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]

let cc_sarq_1 = test_machine  
    [InsB0 (Movq, [(Imm (Lit 0xFFFFFFFFFFFFFFFFL)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Sarq, [~$0; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]

let cc_sarq_2 = test_machine  
    [InsB0 (Movq, [~$424242; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Sarq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]

let cc_sarq_3 = test_machine  
    [InsB0 (Movq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Sarq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]

let cc_sarq_4 = test_machine  
    [InsB0 (Movq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Sarq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]


let cc_shlq_1 = test_machine  
    [InsB0 (Movq, [~$424242; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Shlq, [~$0; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]

let cc_shlq_2 = test_machine  
    [InsB0 (Movq, [(Imm (Lit 0x3FFFFFFFFFFFFFFFL)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Shlq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]

let cc_shlq_3 = test_machine  
    [InsB0 (Movq, [(Imm (Lit 0x7FFFFFFFFFFFFFFFL)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Shlq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]

let cc_shlq_4 = test_machine  
    [InsB0 (Movq, [(Imm (Lit 0x7FFFFFFFFFFFFFFFL)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Shlq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]


let cc_shrq_1 = test_machine  
    [InsB0 (Movq, [~$424242; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Shrq, [~$0; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]

let cc_shrq_2 = test_machine  
    [InsB0 (Movq, [~$(-1); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Shrq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]

let cc_shrq_3 = test_machine  
    [InsB0 (Movq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Shrq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]

let cc_shrq_4 = test_machine  
    [InsB0 (Movq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Shrq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]

(* additional overflow tests *)

let cso_mult_1 = test_machine  
    [InsB0 (Movq, [(Imm (Lit 0x4000000000000000L)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Imulq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]

let cso_add_1 = test_machine  
    [InsB0 (Movq, [(Imm (Lit 0x7FFFFFFFFFFFFFFFL)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Addq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]

let cso_sub_1 = test_machine  
    [InsB0 (Movq, [(Imm (Lit 0x8000000000000000L)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Subq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]

let student_instruction_tests_christian_part1 =
  [ ("cc_sarq_0", cc_from_to 2 cc_sarq_0 (false, false, false) (false, false, false));
    ("cc_sarq_1", cc_from_to 2 cc_sarq_1 (false, false, false) (false, false, false));
    ("cc_sarq_2", cc_from_to 2 cc_sarq_2 (true, false, false) (false, false, false));
    ("cc_sarq_4", cc_from_to 2 cc_sarq_4 (true, false, false) (true, false, true));
    ("cc_shlq_1", cc_from_to 2 cc_shlq_1 (true, false, true) (true, false, true));
    ("cc_shlq_2", cc_from_to 2 cc_shlq_2 (false, false, false) (false, false, false));
    ("cc_shlq_3", cc_from_to 2 cc_shlq_3 (false, false, false) (true, true, false));
    ("cc_shlq_4", cc_from_to 2 cc_shlq_4 (false, false, false) (false, true, false));
    ("cc_shrq_1", cc_from_to 2 cc_shrq_1 (false, false, false) (false, false, false));
    ("cc_shrq_2", cc_from_to 2 cc_shrq_2 (false, false, false) (true, false, false));
    ("cc_shrq_3", cc_from_to 2 cc_shrq_3 (false, false, false) (false, false, true));
    ("cc_shrq_4", cc_from_to 2 cc_shrq_4 (false, false, false) (false, false, true));
    ("cc_mult_1", cso_test 2 cso_mult_1 true); 
    ("cc_add_1", cso_test 2 cso_add_1 true); 
    ("cc_sub_1", cso_test 2 cso_sub_1 true); 
  ]

(* additional functional tests *)

let setb_1 = test_machine
    [InsB0 (Movq, [(Imm (Lit 0xFFFFFFFFFFFFFFFFL)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Cmpq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Set Le, [~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]

let setb_2 = test_machine
    [InsB0 (Movq, [(Imm (Lit 0xFFFFFFFFFFFFFFFFL)); Ind2 Rsp]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Addq, [~$7; ~%Rsp]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Cmpq, [~$2; ~$1]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Set Gt, [Ind2 Rsp]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]

let student_instruction_tests_christian_part2 = [
  ("setb_1", machine_test "only to change the lowest byte" 3 setb_1 (fun m -> 
       m.regs.(rind Rax) = 0xFFFFFFFFFFFFFF01L
     ));  
  ("setb_2", machine_test "only to change the lowest byte" 4 setb_2 (fun m -> 
       m.mem.(0xffff) = Byte(Char.chr 0)
     ));  
]

(* ##### end: tests christian ##### *)

let provided_tests : suite = [
  Test ("student_instruction_tests_flo", student_instruction_tests_flo);
  Test ("student_instruction_tests_philippe", student_instruction_tests_philippe);
  Test ("student_instruction_tests_jan", student_instruction_tests_jan);
  Test ("Additonal cc tests", student_instruction_tests_christian_part1);
  Test ("Additonal functionality tests", student_instruction_tests_christian_part2);
  Test ("Student-Provided Big Test for Part III: Score recorded as PartIIITestCase", []);
] 
