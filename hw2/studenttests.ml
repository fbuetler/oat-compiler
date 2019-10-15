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
    | Set _ -> "Set"
    | Leaq -> "Leaq"
    | Movq -> "Movq"
    | Pushq -> "Pushq"
    | Popq -> "Popq"
    | Cmpq -> "Cmpq"
    | Callq -> "Callq"
    | Retq -> "Retq"
    | Jmp -> "Jmp"
    | J _ -> "J"
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

let binary_search n = [ gtext "main" 
                          [ Movq, [~$$"data"; ~%Rbx] (* base *)
                          ; Movq,  [~$n; ~%Rdi] (* searched val *)
                          ; Callq, [~$$"search"]
                          ; Retq, []
                          ]
                      ; text "search"
                          [ Movq, [~$0; ~%R08] (* left *)
                          ; Movq, [Ind2 (Rbx); ~%R09] (* right *)
                          ; Addq, [Imm (Lit 8L); ~%Rbx] (* start of array *)
                          ; Jmp, [~$$"search.loop"]
                          ]
                      ; text "search.loop"
                          [ Cmpq, [~%R09; ~%R08] 
                          ; J Gt, [~$$"exit.notfound"] (* if l > r then exit  *)
                          ; Movq, [~%R09; ~%R10] (* middle *)
                          ; Addq, [~%R08; ~%R10] (* l+r *)
                          ; Sarq, [~$1; ~%R10]  (* /= 2 *)
                          ; Movq, [~$0; ~%R11]  (* reset m *)
                          ; Addq, [~%R10; ~%R11] (* copy m to preserve it*)
                          ; Shlq, [~$3; ~%R11] (* jump blocks *)
                          ; Addq, [~%Rbx; ~%R11] (* calc offset = base + m*)
                          ; Cmpq, [Ind2 (R11); ~%Rdi]
                          ; J Lt, [~$$"search.less"] (* val < a[middle] then *)
                          ; J Gt, [~$$"search.greater"] (* val > a[middle] *)
                          ; Movq, [~%R10; ~%Rax] (* val == a[middle] *)
                          ; Retq, []
                          ]
                      ; text "search.less"
                          [ Decq, [~%R10]
                          ; Movq, [~%R10; ~%R09]
                          ; Jmp,  [~$$"search.loop"]
                          ]
                      ; text "search.greater"
                          [ Incq, [~%R10]
                          ; Movq, [~%R10; ~%R08]
                          ; Jmp,  [~$$"search.loop"]
                          ]
                      ; text "exit.notfound"
                          [ Movq,  [~$(-1); ~%Rax]
                          ; Retq,  []
                          ]
                      ; data "data"
                          [ Quad (Lit 9L) (* length: 10 - 1 *)
                          ; Quad (Lit 7L)
                          ; Quad (Lit 13L)
                          ; Quad (Lit 27L)
                          ; Quad (Lit 33L)
                          ; Quad (Lit 42L)
                          ; Quad (Lit 54L)
                          ; Quad (Lit 61L)
                          ; Quad (Lit 69L)
                          ; Quad (Lit 82L)
                          ; Quad (Lit 99L)
                          ]
                      ]

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
  make_bin_op_test Shrq 1L (-1L) 0x7fffffffffffffffL {fo=true; fs=false; fz=false};
  make_bin_op_test Shlq 1L 0L 0L {fo=false; fs=false; fz=true};
  make_bin_op_test Shlq 1L 0xc000000000000000L 0x8000000000000000L {fo=false; fs=true; fz=false};

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
              && int64_of_sbytes (sbyte_list m.mem (mem_size-16)) = Int64.add mem_bot ins_size
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

(* additional functional tests *)

let setb_1 = test_machine
    [InsB0 (Movq, [(Imm (Lit 0xFFFFFFFFFFFFFFFFL)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Cmpq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Set Le, [~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]

let student_instruction_tests_christian = [
  ("cc_sarq_0", cc_from_to 2 cc_sarq_0 (false, false, false) (false, false, false));
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
  ("setb_1", machine_test "only to change the lowest byte" 3 setb_1 (fun m -> 
       m.regs.(rind Rax) = 0xFFFFFFFFFFFFFF01L
     ));  
]
(* ##### end: tests christian ##### *)

(* ##### start: tests yaris ##### *)

let sbytes_of_ins (instructions:ins list) : sbyte list =
  List.concat @@ List.map (fun i -> [InsB0 i;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]) instructions

(** Creates machine out of given instructions. *)
let machine_of_ins (instructions:ins list) : mach =
  sbytes_of_ins instructions |> test_machine

(** Creates machine out of given instructions and data. *)
let machine_of_ins2 (instructions:ins list) (data:sbyte list) : mach =
  sbytes_of_ins instructions @ data |> test_machine

(** Check if memory has given value at given address. *)
let check_memory (memory:mem) (address:int64) (value:int64) : bool =
  value = int64_of_sbytes @@ Array.to_list @@ Array.sub memory (Option.get @@ map_addr address) 8

let b0 = Byte '\x00'
let bff = Byte '\xff'

let student_instruction_tests_yaris = [
  ("movq1", machine_test "rax=5" 1 (machine_of_ins [Movq, [~$5; ~%Rax]])
     (fun {regs} -> regs.(rind Rax) = 5L));
  ("movq2", machine_test "rax=*0x400010=65" 2 (machine_of_ins [
       Movq, [~$65; Ind1 (Lit 0x400010L)];
       Movq, [Ind1 (Lit 0x400010L); ~%Rax];
     ]) (fun {regs;mem} -> regs.(rind Rax) = 65L && mem.(16) = Byte 'A'));
  ("movq3", machine_test "rax=*0x400018=66" 3 (machine_of_ins [
       Movq, [~$0x400018; ~%Rax];
       Movq, [~$66; Ind2 Rax];
       Movq, [Ind2 Rax; ~%Rax];
     ]) (fun {regs;mem} -> regs.(rind Rax) = 66L && mem.(24) = Byte 'B'));
  ("movq4", machine_test "rax=*0x400020=67" 3 (machine_of_ins [
       Movq, [~$0x400018; ~%Rax];
       Movq, [~$67; Ind3 (Lit 8L, Rax)];
       Movq, [Ind3 (Lit 8L, Rax); ~%Rax];
     ]) (fun {regs;mem} -> regs.(rind Rax) = 67L && mem.(32) = Byte 'C'));
  ("movq5", machine_test "rax=*0x400021=MIN_INT" 3 (machine_of_ins [
       Movq, [~$0x400018; ~%Rax];
       Movq, [~$0x80; Ind3 (Lit 16L, Rax)];
       Movq, [Ind3 (Lit 9L, Rax); ~%Rax];
     ]) (fun {regs;mem} -> regs.(rind Rax) = Int64.min_int && mem.(40) = Byte '\x80'));
  (* Segfault on invalid memory access *)
  ("movq6", fun () -> try (machine_test "segfault" 1 (machine_of_ins [
       Movq, [Ind1 (Lit 0x410000L); ~%Rax];
     ]) (fun _ -> true) ());
        failwith "bad address" with X86lite_segfault -> ());
  ("movq7", fun () -> try (machine_test "segfault" 2 (machine_of_ins [
       Movq, [~$0x3ffffff; ~%Rax];
       Movq, [Ind2 Rax; ~%Rax];
     ]) (fun _ -> true) ());
        failwith "bad address" with X86lite_segfault -> ());
  ("movq8", fun () -> try (machine_test "segfault" 2 (machine_of_ins [
       Movq, [~$0x410000; ~%Rax];
       Movq, [Ind3 (Lit (-7L), Rax); ~%Rax];
     ]) (fun _ -> true) ());
        failwith "bad address" with X86lite_segfault -> ());

  ("negq1", machine_test "rax=-5" 2 (machine_of_ins [
       Movq, [~$5; ~%Rax];
       Negq, [~%Rax];
     ]) (fun {regs} -> regs.(rind Rax) = -5L));
  ("negq2", machine_test "rax=MIN_INT" 2 (machine_of_ins [
       Movq, [Imm (Lit Int64.min_int); ~%Rax];
       Negq, [~%Rax];
     ]) (fun {flags;regs} -> regs.(rind Rax) = Int64.min_int &&
                             flags.fo && flags.fs && not flags.fz));
  ("negq3", machine_test "rax=MIN_INT+1" 2 (machine_of_ins [
       Movq, [Imm (Lit Int64.max_int); ~%Rax];
       Negq, [~%Rax];
     ]) (fun {flags;regs} -> regs.(rind Rax) = Int64.add Int64.min_int 1L &&
                             not flags.fo && flags.fs && not flags.fz));

  ("incq1", machine_test "rax=5" 2 (machine_of_ins [
       Movq, [~$4; ~%Rax];
       Incq, [~%Rax];
     ]) (fun {regs} -> regs.(rind Rax) = 5L));
  ("incq2", machine_test "rax=MIN_INT" 2 (machine_of_ins [
       Movq, [Imm (Lit Int64.max_int); ~%Rax];
       Incq, [~%Rax];
     ]) (fun {flags;regs} -> regs.(rind Rax) = Int64.min_int &&
                             flags.fo && flags.fs && not flags.fz));

  ("decq1", machine_test "rax=5" 2 (machine_of_ins [
       Movq, [~$6; ~%Rax];
       Decq, [~%Rax];
     ]) (fun {regs} -> regs.(rind Rax) = 5L));
  ("decq2", machine_test "rax=MAX_INT" 2 (machine_of_ins [
       Movq, [Imm (Lit Int64.min_int); ~%Rax];
       Decq, [~%Rax];
     ]) (fun {flags;regs} -> regs.(rind Rax) = Int64.max_int &&
                             flags.fo && not flags.fs && not flags.fz));

  ("addq1", machine_test "rax=55" 2 (machine_of_ins [
       Movq, [~$4; ~%Rax];
       Addq, [~$51; ~%Rax];
     ]) (fun {regs} -> regs.(rind Rax) = 55L));
  ("addq2", machine_test "rax=MIN_INT+4" 2 (machine_of_ins [
       Movq, [Imm (Lit Int64.max_int); ~%Rax];
       Addq, [~$5; ~%Rax];
     ]) (fun {flags;regs} -> regs.(rind Rax) = Int64.add Int64.min_int 4L &&
                             flags.fo && flags.fs && not flags.fz));

  ("subq1", machine_test "rax=55" 2 (machine_of_ins [
       Movq, [~$100; ~%Rax];
       Subq, [~$45; ~%Rax];
     ]) (fun {regs} -> regs.(rind Rax) = 55L));
  ("subq2", machine_test "rax=MIN_INT" 2 (machine_of_ins [
       Movq, [Imm (Lit (Int64.add Int64.min_int 3L)); ~%Rax];
       Subq, [~$3; ~%Rax];
     ]) (fun {flags;regs} -> regs.(rind Rax) = Int64.min_int &&
                             not flags.fo && flags.fs && not flags.fz));
  ("subq3", machine_test "rax=MAX_INT" 2 (machine_of_ins [
       Movq, [Imm (Lit (Int64.add Int64.min_int 3L)); ~%Rax];
       Subq, [~$4; ~%Rax];
     ]) (fun {flags;regs} -> regs.(rind Rax) = Int64.max_int &&
                             flags.fo && not flags.fs && not flags.fz));
  ("subq4", machine_test "rax=0" 2 (machine_of_ins [
       Movq, [Imm (Lit Int64.min_int); ~%Rax];
       Subq, [Imm (Lit Int64.min_int); ~%Rax];
     ]) (fun {flags;regs} -> regs.(rind Rax) = 0L &&
                             flags.fo && not flags.fs && flags.fz));

  ("cmpq1", machine_test "rax=100" 2 (machine_of_ins [
       Movq, [~$100; ~%Rax];
       Cmpq, [~$45; ~%Rax];
     ]) (fun {regs} -> regs.(rind Rax) = 100L));
  ("cmpq2", machine_test "rax=MIN_INT+3" 2 (machine_of_ins [
       Movq, [Imm (Lit (Int64.add Int64.min_int 3L)); ~%Rax];
       Cmpq, [~$3; ~%Rax];
     ]) (fun {flags;regs} -> regs.(rind Rax) = Int64.add Int64.min_int 3L &&
                             not flags.fo && flags.fs && not flags.fz));
  ("cmpq3", machine_test "rax=MAX_INT+3" 2 (machine_of_ins [
       Movq, [Imm (Lit (Int64.add Int64.min_int 3L)); ~%Rax];
       Cmpq, [~$4; ~%Rax];
     ]) (fun {flags;regs} -> regs.(rind Rax) = Int64.add Int64.min_int 3L &&
                             flags.fo && not flags.fs && not flags.fz));
  ("cmpq4", machine_test "rax=MIN_INT" 2 (machine_of_ins [
       Movq, [Imm (Lit Int64.min_int); ~%Rax];
       Cmpq, [Imm (Lit Int64.min_int); ~%Rax];
     ]) (fun {flags;regs} -> regs.(rind Rax) = Int64.min_int &&
                             flags.fo && not flags.fs && flags.fz));
  (* Compq does not need a destination *)
  ("cmpq5", machine_test "OF=1 FS=0 FZ=1" 1 (machine_of_ins [
       Cmpq, [Imm (Lit Int64.min_int); Imm (Lit Int64.min_int)];
     ]) (fun {flags} -> flags.fo && not flags.fs && flags.fz));

  ("imulq1", machine_test "rax=55" 2 (machine_of_ins [
       Movq, [~$5; ~%Rax];
       Imulq, [~$11; ~%Rax];
     ]) (fun {regs} -> regs.(rind Rax) = 55L));
  ("imulq2", machine_test "rax=MAX_INT rbx=MIN_INT" 4 (machine_of_ins [
       Movq, [Imm (Lit Int64.max_int); ~%Rax];
       Imulq, [~$1; ~%Rax];
       Movq, [Imm (Lit Int64.min_int); ~%Rbx];
       Imulq, [~$1; ~%Rbx];
     ]) (fun {flags;regs} -> not flags.fo && (* ZF and SF undefined *)
                             regs.(rind Rax) = Int64.max_int &&
                             regs.(rind Rbx) = Int64.min_int));
  ("imulq3", machine_test "rax=4 and OF" 2 (machine_of_ins [
       Movq, [Imm (Lit 0x4000000000000001L); ~%Rax];
       Imulq, [~$4; ~%Rax];
     ]) (fun {flags;regs} -> flags.fo (* ZF and SF undefined *) &&
                             regs.(rind Rax) = 4L));
  ("imulq4", machine_test "rax=0 and OF" 2 (machine_of_ins [
       Movq, [Imm (Lit Int64.min_int); ~%Rax];
       Imulq, [~$2; ~%Rax];
     ]) (fun {flags;regs} -> flags.fo (* ZF and SF undefined *) &&
                             regs.(rind Rax) = 0L));

  ("notq1", machine_test "rax=-1" 2 (machine_of_ins [
       Movq, [~$0; ~%Rax];
       Notq, [~%Rax];
     ]) (fun {regs} -> regs.(rind Rax) = -1L));
  ("notq2", machine_test "rax=MIN_INT" 2 (machine_of_ins [
       Movq, [Imm (Lit Int64.max_int); ~%Rax];
       Notq, [~%Rax];
     ]) (fun {regs} -> regs.(rind Rax) = Int64.min_int));

  ("andq1", machine_test "rax=0b00101" 2 (machine_of_ins [
       Movq, [~$0b00111; ~%Rax];
       Andq, [~$0b01101; ~%Rax];
     ]) (fun {flags;regs} -> regs.(rind Rax) = 0b00101L &&
                             not flags.fo && not flags.fs && not flags.fz));
  ("andq2", machine_test "rax=0" 2 (machine_of_ins [
       Movq, [Imm (Lit Int64.min_int); ~%Rax];
       Andq, [Imm (Lit Int64.max_int); ~%Rax];
     ]) (fun {flags;regs} -> regs.(rind Rax) = 0L &&
                             not flags.fo && not flags.fs && flags.fz));

  ("orq1", machine_test "rax=0b01111" 2 (machine_of_ins [
       Movq, [~$0b00111; ~%Rax];
       Orq,  [~$0b01101; ~%Rax];
     ]) (fun {flags;regs} -> regs.(rind Rax) = 0b01111L &&
                             not flags.fo && not flags.fs && not flags.fz));
  ("orq2", machine_test "rax=-1" 2 (machine_of_ins [
       Movq, [Imm (Lit Int64.min_int); ~%Rax];
       Orq,  [Imm (Lit Int64.max_int); ~%Rax];
     ]) (fun {flags;regs} -> regs.(rind Rax) = -1L &&
                             not flags.fo && flags.fs && not flags.fz));

  ("xorq1", machine_test "rax=0b01010" 2 (machine_of_ins [
       Movq, [~$0b00111; ~%Rax];
       Xorq, [~$0b01101; ~%Rax];
     ]) (fun {flags;regs} -> regs.(rind Rax) = 0b01010L &&
                             not flags.fo && not flags.fs && not flags.fz));
  ("xorq2", machine_test "rax=-1" 2 (machine_of_ins [
       Movq, [Imm (Lit Int64.min_int); ~%Rax];
       Xorq, [Imm (Lit Int64.max_int); ~%Rax];
     ]) (fun {flags;regs} -> regs.(rind Rax) = -1L &&
                             not flags.fo && flags.fs && not flags.fz));

  ("leaq1", machine_test "rax=0x400000" 1 (machine_of_ins [
       Leaq, [Ind1 (Lit 0x400000L); ~%Rax];
     ]) (fun {regs} -> regs.(rind Rax) = 0x400000L));
  ("leaq2", machine_test "rax=0x400010" 2 (machine_of_ins [
       Movq, [~$0x400010; ~%Rax];
       Leaq, [Ind2 Rax; ~%Rax];
     ]) (fun {regs} -> regs.(rind Rax) = 0x400010L));
  ("leaq3", machine_test "rax=0x400020" 2 (machine_of_ins [
       Movq, [~$0x400010; ~%Rax];
       Leaq, [Ind3 (Lit 0x10L, Rax); ~%Rax];
     ]) (fun {regs} -> regs.(rind Rax) = 0x400020L));
  (* Invalid addresses do not segfault until accessed *)
  ("leaq4", machine_test "rax=0" 2 (machine_of_ins [
       Movq, [~$0; ~%Rax];
       Leaq, [Ind2 Rax; ~%Rax];
     ]) (fun {regs} -> regs.(rind Rax) = 0L));
  ("leaq5", machine_test "rax=0x410000" 1 (machine_of_ins [
       Leaq, [Ind1 (Lit 0x410000L); ~%Rax];
     ]) (fun {regs} -> regs.(rind Rax) = 0x410000L));
  ("leaq6", fun () -> try (machine_test "segfault" 2 (machine_of_ins [
       Leaq, [Ind1 (Lit 0x410000L); ~%Rax];
       Movq, [Ind2 Rax; ~%Rax];
     ]) (fun _ -> true) ());
        failwith "bad address" with X86lite_segfault -> ());
  (* As answered by Yichen Yan on moodle *)
  ("leaq7", machine_test "rax=0x400008L" 1 (machine_of_ins [
       Leaq, [Ind2 Rip; ~%Rax];
     ]) (fun {regs} -> regs.(rind Rax) = 0x400008L));

  ("setb1", machine_test "rax=rdx=r09=1 rbx=rcx=r08=0" 8 (machine_of_ins [
       Movq, [~$0; ~%Rax];
       Cmpq, [~$0; ~$0];
       Set Eq, [~%Rax];
       Set Neq, [~%Rbx];
       Set Gt, [~%Rcx];
       Set Ge, [~%Rdx];
       Set Lt, [~%R08];
       Set Le, [~%R09];
     ]) (fun {regs} ->
      regs.(rind Rax) = 1L && regs.(rind Rdx) = 1L && regs.(rind R09) = 1L &&
      regs.(rind Rbx) = 0L && regs.(rind Rcx) = 0L && regs.(rind R08) = 0L));
  ("setb2", machine_test "rbx=r08=r09=1 rax=rcx=rdx=0" 8 (machine_of_ins [
       Movq, [~$0; ~%Rax];
       Cmpq, [~$5; ~$(-5)];
       Set Eq, [~%Rax];
       Set Neq, [~%Rbx];
       Set Gt, [~%Rcx];
       Set Ge, [~%Rdx];
       Set Lt, [~%R08];
       Set Le, [~%R09];
     ]) (fun {regs} ->
      regs.(rind Rbx) = 1L && regs.(rind R08) = 1L && regs.(rind R09) = 1L &&
      regs.(rind Rax) = 0L && regs.(rind Rcx) = 0L && regs.(rind Rdx) = 0L));
  (* setq does only access lowest byte *)
  ("setb3", machine_test "*0x400018=0x01ffffffffffffff" 3 (machine_of_ins [
       Movq, [~$(-1); Ind1 (Lit 0x400018L)];
       Cmpq, [~$0; ~$0];
       Set Eq, [Ind1 (Lit 0x400018L)];
     ]) (fun {mem} -> Array.sub mem 24 8 = [|Byte '\x01';bff;bff;bff;bff;bff;bff;bff|]));
  ("setb4", machine_test "*0x400018=0x00ffffffffffffff" 3 (machine_of_ins [
       Movq, [~$(-1); Ind1 (Lit 0x400018L)];
       Cmpq, [~$0; ~$0];
       Set Neq, [Ind1 (Lit 0x400018L)];
     ]) (fun {mem} -> Array.sub mem 24 8 = [|b0;bff;bff;bff;bff;bff;bff;bff|]));

  ("jmp1", machine_test "rax=5" 3 (machine_of_ins [
       Movq, [~$5; ~%Rax];
       Jmp, [~$0x400000];
       Movq, [~$1337; ~%Rax];
     ]) (fun {regs} -> regs.(rind Rax) = 5L));
  ("jmp2", machine_test "rax=5" 3 (machine_of_ins [
       Movq, [~$5; ~%Rax];
       Jmp, [~$0x400008];
       Movq, [~$1337; ~%Rax];
     ]) (fun {regs} -> regs.(rind Rax) = 5L));
  ("jmp3", machine_test "rax=5" 4 (machine_of_ins [
       Movq, [~$5; ~%Rax];
       Movq, [~$0x400000; ~%Rbx];
       Jmp, [Reg Rbx];
       Movq, [~$1337; ~%Rax];
     ]) (fun {regs} -> regs.(rind Rax) = 5L));

  ("j1", machine_test "rax=5" 4 (machine_of_ins [
       Movq, [~$5; ~%Rax];
       Cmpq, [~$0; ~$0];
       J Eq, [~$0x400000];
       Movq, [~$1337; ~%Rax];
     ]) (fun {regs} -> regs.(rind Rax) = 5L));
  ("j2", machine_test "rax=1337" 4 (machine_of_ins [
       Movq, [~$5; ~%Rax];
       Cmpq, [~$0; ~$0];
       J Neq, [~$0x400000];
       Movq, [~$1337; ~%Rax];
     ]) (fun {regs} -> regs.(rind Rax) = 1337L));

  ("pushq1", machine_test "rsp=0x40fff0 *0x40fff0=5" 1 (machine_of_ins [
       Pushq, [~$5];
     ]) (fun {regs;mem} ->
      regs.(rind Rsp) = Int64.sub mem_top 16L &&
      check_memory mem (Int64.sub mem_top 16L) 5L));
  ("pushq2", machine_test "rsp=0x40ffe0 *0x40fff0=5 *0x40ffe8=13 *0x40ffe0=59" 3 (machine_of_ins [
       Pushq, [~$5];
       Pushq, [~$13];
       Pushq, [~$59];
     ]) (fun {regs;mem} ->
      regs.(rind Rsp) = Int64.sub mem_top 32L &&
      check_memory mem (Int64.sub mem_top 16L) 5L &&
      check_memory mem (Int64.sub mem_top 24L) 13L &&
      check_memory mem (Int64.sub mem_top 32L) 59L));

  ("popq1", machine_test "rax=13 rsp=0x40ffe8 *0x40fff0=5" 3 (machine_of_ins [
       Pushq, [~$5];
       Pushq, [~$13];
       Popq, [~%Rax];
     ]) (fun {regs;mem} ->
      regs.(rind Rax) = 13L &&
      regs.(rind Rsp) = Int64.sub mem_top 16L &&
      check_memory mem (Int64.sub mem_top 16L) 5L));
  ("popq2", machine_test "rax=13 rsp=0x40ffe8 *0x40fff0=5" 5 (machine_of_ins [
       Pushq, [~$5];
       Pushq, [~$13];
       Pushq, [~$59];
       Popq, [~%Rax];
       Popq, [~%Rax];
     ]) (fun {regs;mem} ->
      regs.(rind Rax) = 13L &&
      regs.(rind Rsp) = Int64.sub mem_top 16L &&
      check_memory mem (Int64.sub mem_top 16L) 5L));
  ("popq3", machine_test "rax=-1337 rsp=0x40fff8" 6 (machine_of_ins [
       Pushq, [~$27];
       Pushq, [~$42];
       Popq, [~%Rax];
       Popq, [~%Rax];
       Pushq, [~$(-1337)];
       Popq, [~%Rax];
     ]) (fun {regs;mem} ->
      regs.(rind Rax) = -1337L &&
      regs.(rind Rsp) = Int64.sub mem_top 8L));

  ("callq1", machine_test "rsp=0x40fff0 rip=0x400020 *0x40fff0=0x400008" 1 (machine_of_ins [
       Callq, [~$0x400020];
     ]) (fun {regs;mem} ->
      regs.(rind Rsp) = Int64.sub mem_top 16L &&
      regs.(rind Rip) = 0x400020L &&
      check_memory mem (Int64.sub mem_top 16L) 0x400008L));
  ("callq2", machine_test "rsp=0x40ffe8 rip=0x400030 *0x40fff0=0x400008 *0x40ffe8=0x400010" 2 (machine_of_ins [
       Callq, [~$0x400008];
       Callq, [~$0x400030];
     ]) (fun {regs;mem} ->
      regs.(rind Rsp) = Int64.sub mem_top 24L &&
      regs.(rind Rip) = 0x400030L &&
      check_memory mem (Int64.sub mem_top 16L) 0x400008L &&
      check_memory mem (Int64.sub mem_top 24L) 0x400010L));

  ("retq1", machine_test "rsp=0x40fff8 rip=0x400008 rax=5" 3 (machine_of_ins [
       Callq, [~$0x400010];
       Movq, [~$42; ~%Rax];
       Movq, [~$5; ~%Rax];
       Retq, [];
     ]) (fun {regs;mem} ->
      regs.(rind Rsp) = Int64.sub mem_top 8L &&
      regs.(rind Rip) = 0x400008L &&
      regs.(rind Rax) = 5L));
  (*
  ("retq2", machine_test "rsp=0x410000 rip=exit_addr" 5 (
      let machine = machine_of_ins [
          Callq, [~$0x400010];
          Retq, [];
          Callq, [~$0x400018];
          Retq, [];
        ] in
      (* Set top of the stack to exit_addr (equal to how it is done in load) *)
      write_memory machine.mem (Int64.sub mem_top 8L) exit_addr; machine)
      (fun {regs;mem} ->
         regs.(rind Rsp) = mem_top &&
         regs.(rind Rip) = exit_addr));
    *)
]
(* ##### end: tests yaris ##### *)

(* ##### start: tests manuel ##### *)

let is_sorted = [
  text "is_sorted"
    [ Pushq, [~%Rbx]
    ; Pushq, [~%Rbp]
    ; Movq, [~%Rdi; ~%Rbp]
    ; Movq, [~%Rsi; ~%Rbx]
    ; Jmp, [~$$"sorted_loop"]
    ]
; text "sorted_top"
    [ Cmpq, [Ind3 (Lit 8L, Rbp); Ind2 Rbp]
    ; J Gt, [~$$"not_sorted"]
    ; Addq, [~$8; ~%Rbp]
    ]
; text "sorted_loop"
    [ Cmpq, [~%Rbx; ~%Rbp]
    ; J Lt, [~$$"sorted_top"]
    ; Movq, [~$1; ~%Rax]
    ; Popq, [~%Rbp]
    ; Popq, [~%Rbx]
    ; Retq, []
    ]
; text "not_sorted"
    [ Xorq, [~%Rax; ~%Rax]
    ; Popq, [~%Rbp]
    ; Popq, [~%Rbx]
    ; Retq, []
    ]
]

let quicksort = [
  text "quicksort"
    [ Pushq, [~%Rbx]
    ; Pushq, [~%Rbp]
    ; Pushq, [~%R12]
    ; Movq, [~%Rdi; ~%Rbp]
    ; Movq, [~%Rsi; ~%Rbx]
    ; Cmpq, [~%Rbp; ~%Rbx]
    ; J Le, [~$$"quicksort_exit"]
    ; Callq, [~$$"partition"]
    ; Movq, [~%Rax; ~%R12]
    ; Movq, [~%Rbp; ~%Rdi]
    ; Leaq, [Ind3 (Lit (-8L), R12); ~%Rsi]
    ; Callq, [~$$"quicksort"]
    ; Leaq, [Ind3 (Lit 8L, R12); ~%Rdi]
    ; Movq, [~%Rbx; ~%Rsi]
    ; Callq, [~$$"quicksort"]
    ]
; text "quicksort_exit"
    [ Popq, [~%R12]
    ; Popq, [~%Rbp]
    ; Popq, [~%Rbx]
    ; Retq, []
    ]
; text "partition"
    [ Pushq, [~%Rbx]
    ; Pushq, [~%Rbp]
    ; Pushq, [~%R12]
    ; Pushq, [~%Rdi]
    ; Pushq, [~%Rsi]
    ; Movq, [~%Rdi; ~%Rbp] (* i *)
    ; Leaq, [Ind3 (Lit (-8L), Rsi); ~%Rbx] (* j *)
    ; Movq, [Ind2 Rsi; ~%R12] (* pivot *)
    ]
; text "part_loop"
    [ Jmp, [~$$"i_loop_entry"] ]
; text "i_loop"
    [ Addq, [~$8; ~%Rbp] ]
; text "i_loop_entry"
    [ Cmpq, [~%Rbp; ~%Rsi]
    ; J Le, [~$$"j_loop_entry"]
    ; Cmpq, [Ind2 Rbp; ~%R12]
    ; J Le, [~$$"j_loop_entry"]
    ; Jmp, [~$$"i_loop"]
    ]
; text "j_loop"
    [ Subq, [~$8; ~%Rbx] ]
; text "j_loop_entry"
    [ Cmpq, [~%Rbx; ~%Rdi]
    ; J Ge, [~$$"if_swap"]
    ; Cmpq, [Ind2 Rbx; ~%R12]
    ; J Le, [~$$"j_loop"]
    ]
; text "if_swap"
    [ Cmpq, [~%Rbp; ~%Rbx]
    ; J Le, [~$$"swap_pivot"]
    ; Movq, [~%Rbp; ~%Rdi]
    ; Movq, [~%Rbx; ~%Rsi]
    ; Callq, [~$$"swap"]
    ; Movq, [Ind2 Rsp; ~%Rsi]
    ; Movq, [Ind3 (Lit 8L, Rsp); ~%Rdi]
    ; Jmp, [~$$"part_loop"]
    ]
; text "swap_pivot"
    [ Movq, [~%Rbp; ~%Rdi]
    ; Callq, [~$$"swap"]
    ; Movq, [~%Rbp; ~%Rax]
    ; Popq, [~%Rsi]
    ; Popq, [~%Rdi]
    ; Popq, [~%R12]
    ; Popq, [~%Rbp]
    ; Popq, [~%Rbx]
    ; Retq, []
    ]
; gtext "swap"
    [ Movq, [Ind2 Rdi; ~%Rcx]
    ; Movq, [Ind2 Rsi; Ind2 Rdi]
    ; Movq, [~%Rcx; Ind2 Rsi]
    ; Retq, []
    ]
]

let sorted_test (l: int64 list) : prog =
  let arr = List.map (fun x -> Quad (Lit x)) l in
  let len = (List.length l) - 1 in
  [ data "array" arr
  ; gtext "main"
      [ Leaq, [Ind1 (Lbl "array"); ~%Rdi]
      ; Leaq, [Ind3 (Lit (Int64.of_int (8 * len)), Rdi); ~%Rsi]
      ; Callq, [~$$"is_sorted"]
      ]
  ; text "exit" [ Retq, [] ]
  ]

let quicksort_test (l: int64 list) : prog =
  let arr = List.map (fun x -> Quad (Lit x)) l in
  let len = (List.length l) - 1 in
  [ data "array" arr
  ; gtext "main"
      [ Leaq, [Ind1 (Lbl "array"); ~%Rdi]
      ; Leaq, [Ind3 (Lit (Int64.of_int (8 * len)), Rdi); ~%Rsi]
      ; Pushq, [~%Rsi]
      ; Pushq, [~%Rdi]
      ; Callq, [~$$"quicksort"]
      ; Popq, [~%Rdi]
      ; Popq, [~%Rsi]
      ; Callq, [~$$"is_sorted"]
      ]
  ; text "exit" [ Retq, [] ]
  ]
let exit_test = [text "main" [Popq, [~%Rax]; Pushq, [~%Rax]; Retq, []]]

let student_instruction_tests_manuel = [
  ("quicksort one element", program_test (quicksort_test [10L] @ quicksort @ is_sorted) 1L);
  ("quicksort already sorted",
   program_test (quicksort_test [1L;2L;3L;4L;5L] @ quicksort @ is_sorted) 1L);
  ("quicksort small",
   program_test (quicksort_test [2L;4L;3L;1L;5L] @ quicksort @ is_sorted) 1L);
  ("quicksort big",
   program_test (quicksort_test [83L;39L;453L;55L;37L;42L;38L;418L;61L;42L;32L] @ quicksort @ is_sorted) 1L);
  ("fact6", program_test (factorial_iter 6) 720L);
  ("exit", program_test (exit_test) exit_addr);
  ("is_sorted one element", program_test (sorted_test [10L] @ is_sorted) 1L);
  ("is_sorted short negative", program_test (sorted_test [10L; 8L] @ is_sorted) 0L);
  ("is_sorted short positive", program_test (sorted_test [6L; 8L] @ is_sorted) 1L);
  ("is_sorted long negative", program_test (sorted_test [1L;2L;5L;8L;4L] @ is_sorted) 0L);
  ("is_sorted long positive", program_test (sorted_test [1L;40L;89L;100L] @ is_sorted) 1L);
  ("is_sorted equal elements", program_test (sorted_test [1L;2L;3L;3L;4L] @ is_sorted) 1L);
]

(* ##### end: tests manuel ##### *)

(* ##### start: tests roman ##### *)
let fib_rec n = [ gtext "main"
                    [ Movq,  [~$n; ~%Rdi]
                    ; Callq, [~$$"fib"]
                    ; Retq,  []
                    ]
                ; text "fib"
                    [ Cmpq, [~$1; ~%Rdi] (* compute rdi - 1 *)
                    ; J Le, [~$$"exit"]
                    ; Pushq, [~%Rdi]
                    ; Decq, [~%Rdi]
                    ; Callq, [~$$"fib"] (* recursive call, DO NOT REUSE REGISTERS! *)
                    ; Popq, [~%Rdi]
                    ; Subq, [~$2; ~%Rdi]
                    ; Pushq, [~%Rax] (* save first rec-result to stack, cuz regs might be overwritten *)
                    ; Callq, [~$$"fib"] (* recursive call, DO NOT REUSE REGISTERS! *)
                    ; Popq, [~%R08]
                    ; Addq, [~%R08; ~%Rax]
                    ; Retq,  []
                    ]
                ; text "exit"
                    [ Movq,  [~$1; ~%Rax]
                    ; Retq,  []
                    ]
                ]

(* gcd_loop: computing the gcd of 2 numbers with a loop instead of recursively *)
let gcd_loop a b = [ gtext "main"
                       (* move a to rdi, and b to rsi *)
                       [ Movq,  [~$a; ~%Rdi]
                       ; Movq,  [~$b; ~%Rsi]
                       ; Cmpq, [~$0; ~%Rsi]
                       ; J Ge, [Ind3 (Lit 8L, Rip)] (* If input was positive: no negation needed *)
                       ; Negq, [~%Rsi] (* otherwise negate input / make it positive *)
                       ; Cmpq, [~$0; ~%Rdi] (* and the same for the other input *)
                       ; J Ge, [Ind3 (Lit 8L, Rip)]
                       ; Negq, [~%Rdi]
                       ; Callq, [~$$"gcd"]
                       ; Retq,  []
                       ]
                   ; text "gcd"
                       [ Cmpq, [~%Rdi; ~%Rsi] (* we want a > b, if that is not the case: swap them *)
                       ; J Le, [Ind3 (Lit 32L, Rip)]
                       (* swap section *)
                       ; Movq, [~%Rsi; ~%R10]
                       ; Movq, [~%Rdi; ~%Rsi]
                       ; Movq, [~%R10; ~%Rdi]
                       (* if the smaller one of the variables is 0, then we return :D *)
                       ; Cmpq, [~$0; ~%Rsi]
                       ; J Eq, [~$$"return"]
                       (* otherwise sub b from a, then get back to loop *)
                       ; Subq, [~%Rsi; ~%Rdi]
                       ; Jmp, [~$$"gcd"]
                       ]
                   ; text "return" (* return section: return a *)
                       [ Movq, [~%Rdi; ~%Rax]
                       ; Retq, []
                       ]
                   ]
(* NOTE: to show off how good our assemble function is, we arranged the sections as bad as possible *)
let gcd_rec a b = [ text "exita"
                      [ Movq,  [~%Rsi; ~%Rax]
                      ; Retq,  []
                      ]
                  ; gtext "main"
                      [ Movq,  [~$a; ~%Rdi]
                      ; Movq,  [~$b; ~%Rsi]
                      (* if negaitve inputs were given, negate them (make them positive) *)
                      ; Cmpq, [~$0; ~%Rsi]
                      ; J Ge, [Ind3 (Lit 8L, Rip)] (* If input was positive: no negation needed *)
                      ; Negq, [~%Rsi] (* otherwise negate input / make it positive *)
                      ; Cmpq, [~$0; ~%Rdi] (* and the same for the other input *)
                      ; J Ge, [Ind3 (Lit 8L, Rip)]
                      ; Negq, [~%Rdi]
                      ; Callq, [~$$"gcd"]
                      ; Retq,  []
                      ]
                  ; text "exitb"
                      [ Movq,  [~%Rdi; ~%Rax]
                      ; Retq,  []
                      ]
                  ; text "gcd"
                      (* ARGUMENTS: b == Rsi, a == Rdi *)
                      [ Cmpq, [~$0; ~%Rsi] (* compare b with 0  *)
                      ; J Eq, [~$$"exitb"] (* If b = 0: return a*)
                      ; Cmpq, [~$0; ~%Rdi] (* compare a with 0  *)
                      ; J Eq, [~$$"exita"] (* If a = 0: return b*)
                      ; Cmpq, [~%Rsi; ~%Rdi] (* compute a - b *)
                      ; J Le, [Ind3 (Lit 24L, Rip)] (* if a <= b, then skip next 3 instructions*)
                      (* CASE: a >= b: return gcd(a - b, b) *)
                      ; Subq, [~%Rsi; ~%Rdi]
                      ; Callq, [~$$"gcd"]
                      ; Retq,  []
                      (* CASE: b > a: return gcd(a, b - a) *)
                      ; Subq, [~%Rdi; ~%Rsi]
                      ; Callq, [~$$"gcd"]
                      ; Retq,  []
                      ]
                  ]

let program_test (p:prog) (ans:int64) () =
  let res = assemble p |> load |> run in
  if res <> ans
  then failwith (Printf.sprintf("Expected %Ld but got %Ld") ans res)
  else ()

let student_instruction_tests_roman = [
  ("fib20", program_test (fib_rec 20) 10946L);
  ("fib-1", program_test (fib_rec (-1)) 1L);
  ("fib1", program_test (fib_rec 1) 1L);
  ("fib2", program_test (fib_rec 2) 2L);
  ("fib3", program_test (fib_rec 3) 3L);
  ("fib4", program_test (fib_rec 4) 5L);
  ("fib5", program_test (fib_rec 5) 8L);
  ("fib15", program_test (fib_rec 15) 987L);
  ("fib20", program_test (fib_rec 20) 10946L);
  ("fib25", program_test (fib_rec 25) 121393L);
(*
  ("rec_gcd1", program_test (gcd_rec 1 1) 1L);
  ("rec_gcd2", program_test (gcd_rec 2 3) 1L);
  ("rec_gcd3", program_test (gcd_rec 12 21) 3L);
  ("rec_gcd4", program_test (gcd_rec 78624 20736) 864L);
  (* repeat the previous three tests, just with some negative inputs*)
  ("rec_gcd5", program_test (gcd_rec (-2) 3) 1L);
  ("rec_gcd6", program_test (gcd_rec 12 (-21)) 3L);
  ("rec_gcd7", program_test (gcd_rec (-78624) (-20736)) 864L);
  ("it_gcd1", program_test (gcd_loop 1 1) 1L);
  ("it_gcd2", program_test (gcd_loop 2 3) 1L);
  ("it_gcd3", program_test (gcd_loop 12 21) 3L);
  ("it_gcd4", program_test (gcd_loop 78624 20736) 864L);
  (* repeat the previous three tests, just with some negative inputs*)
  ("it_gcd5", program_test (gcd_loop (-2) 3) 1L);
  ("it_gcd6", program_test (gcd_loop 12 (-21)) 3L);
  ("it_gcd7", program_test (gcd_loop (-78624) (-20736)) 864L);
*)
]
(* ##### end: tests roman ##### *)

(* ##### end: tests christian ##### *)

let run_debug (m:mach) : int64 = 
  let call_depth = ref 0 in
  while m.regs.(rind Rip) <> exit_addr do
    Printf.printf "%d %s\n" !call_depth (string_of_ins @@ get_instr m);
    begin match get_instr m with
      | Callq, _ -> call_depth := !call_depth + 1
      | Retq, _ -> call_depth := !call_depth - 1
      | _ -> ()
    end;
    step m
  done;
  m.regs.(rind Rax)

let program_test_debug (p:prog) (ans:int64) () =
  print_string ("begin program_test_debug\n");
  let res = assemble p |> load |> run_debug in
  if res <> ans
  then failwith (Printf.sprintf("Expected %Ld but got %Ld") ans res)
  else ();
  print_string ("\n")

let make_sub_parse_test (inp:string) (ans:int) = 
  let prog = [ gtext "main"
                 [ Movq, [~$$"input"; ~%Rcx]
                 ; Callq,  [~$$"takeExpr"]
                 ; Retq,  []
                 ]

             ; text "takeExpr"
                 [ Callq, [~$$"takeSide"]
                 ; Movq, [Ind2 Rcx; ~%Rdx]
                 ; Andq, [~$255; ~%Rdx]
                 ; Cmpq, [~$45; ~%Rdx]
                 ; J Neq, [~$$"takeExpr.lit"]
                 ; Incq, [~%Rcx]
                 ; Pushq, [~%Rax]
                 ; Callq, [~$$"takeSide"]
                 ; Popq, [~%R08]
                 ; Subq, [~%Rax; ~%R08]
                 ; Movq, [~%R08; ~%Rax]
                 ; Jmp, [~$$"takeExpr.end"]
                 ]
             ; text "takeExpr.lit"
                 [ Popq, [~%R08]
                 ]
             ; text "takeExpr.end"
                 [ Retq,  []
                 ]

             ; text "takeSide"
                 [ Movq, [Ind2 Rcx; ~%Rdx]
                 ; Andq, [~$255; ~%Rdx]
                 ; Cmpq, [~$40; ~%Rdx]
                 ; J Neq, [~$$"takeSide.lit"]
                 ]
             ; text "takeSide.paren"
                 [ Incq, [~%Rcx]
                 ; Callq, [~$$"takeExpr"]
                 ; Incq, [~%Rcx]
                 (* ; Movq, [~$5; ~%Rax] *)
                 ; Jmp, [~$$"takeSide.end"]
                 ]
             ; text "takeSide.lit"
                 [ Callq, [~$$"takeLit"]
                 ; Jmp, [~$$"takeSide.end"]
                 ]
             ; text "takeSide.end"
                 [ Retq,  []
                 ]

             ; text "takeLit"
                 [  Pushq, [~%R08]
                 ; Movq, [~$0; ~%Rax]
                 ]
             ; text "takeLit.loopStart"
                 [ Movq, [Ind2 Rcx; ~%Rdx]
                 ; Andq, [~$255; ~%Rdx]
                 ; Subq, [~$48; ~%Rdx]
                 ; J Lt, [~$$"takeLit.end"]
                 ; Movq, [~%Rdx; ~%R08]
                 ; Subq, [~$9; ~%R08]
                 ; J Gt, [~$$"takeLit.end"]
                 ; Imulq, [~$10; ~%Rax]
                 ; Addq, [~%Rdx; ~%Rax]
                 ; Incq, [~%Rcx]
                 ; Jmp, [~$$"takeLit.loopStart"]
                 ]
             ; text "takeLit.end"
                 [ Popq, [~%R08]
                 ; Retq,  []
                 ]

             ; data "input"
                 [ Asciz inp ]
             ] in
  ("sub_parse " ^ inp, program_test prog (Int64.of_int ans))


let provided_tests : suite = [
  Test ("student_instruction_tests_flo", student_instruction_tests_flo);
  Test ("student_instruction_tests_philippe", student_instruction_tests_philippe);
  Test ("student_instruction_tests_jan", student_instruction_tests_jan);
  Test ("student_instruction_tests_christian", student_instruction_tests_christian);
  Test ("student_instruction_tests_roman", student_instruction_tests_roman);
  Test ("student_instruction_tests_yaris", student_instruction_tests_yaris);
  Test ("student_instruction_tests_manuel", student_instruction_tests_manuel);
  Test ("Student-Provided Big Test for Part III: Score recorded as PartIIITestCase", [
      make_sub_parse_test "5" 5;
      make_sub_parse_test "84" 84;
      make_sub_parse_test "84walrus" 84;
      make_sub_parse_test "(5)" 5;
      make_sub_parse_test "5-3" 2;
      make_sub_parse_test "(5-3)" 2;
      make_sub_parse_test "(10-1)-5" 4;
      make_sub_parse_test "9-(10-5)" 4;
      make_sub_parse_test "9-((12-2)-5)" 4;
      make_sub_parse_test "(10-1)-5" 4;
      make_sub_parse_test "9-(10-5)" 4;
      make_sub_parse_test "9-((12-2)-5)" 4;
      make_sub_parse_test "(7-(1-2))-((12-2)-5)" 3;
      make_sub_parse_test "((7-(1-2))-((12-2)-5))" 3;
      make_sub_parse_test "(1-(1-(1-(1-1))))" 1;
      make_sub_parse_test "((((1-1)-1)-1)-1)" (-3);

      (* array = [7,13,27,33,42,54,61,69,82,99] *)
      ("binarysearch7", program_test (binary_search 7) 0L);
      ("binarysearch13", program_test (binary_search 13) 1L);
      ("binarysearch27", program_test (binary_search 27) 2L);
      ("binarysearch33", program_test (binary_search 33) 3L);
      ("binarysearch42", program_test (binary_search 42) 4L);
      ("binarysearch54", program_test (binary_search 54) 5L);
      ("binarysearch61", program_test (binary_search 61) 6L);
      ("binarysearch69", program_test (binary_search 69) 7L);
      ("binarysearch82", program_test (binary_search 82) 8L);
      ("binarysearch99", program_test (binary_search 99) 9L);
      ("binarysearch1000", program_test (binary_search 1000) (-1L));
      ("binarysearch1", program_test (binary_search 1) (-1L));
      ("binarysearch0", program_test (binary_search 0) (-1L));
      ("binarysearch", program_test (binary_search (-1)) (-1L));
      ("binarysearch-22", program_test (binary_search (-22)) (-1L));
    ]);
] 
