(* X86lite Simulator *)

(* See the documentation in the X86lite specification, available on the 
   course web pages, for a detailed explanation of the instruction
   semantics.
*)

open X86

(* simulator machine state -------------------------------------------------- *)

let mem_bot = 0x400000L          (* lowest valid address *)
let mem_top = 0x410000L          (* one past the last byte in memory *)
let mem_size = Int64.to_int (Int64.sub mem_top mem_bot)
let nregs = 17                   (* including Rip *)
let ins_size = 8L                (* assume we have a 8-byte encoding *)
let exit_addr = 0xfdeadL         (* halt when m.regs(%rip) = exit_addr *)

(* Your simulator should raise this exception if it tries to read from or
   store to an address not within the valid address space. *)
exception X86lite_segfault

(* The simulator memory maps addresses to symbolic bytes.  Symbolic
   bytes are either actual data indicated by the Byte constructor or
   'symbolic instructions' that take up four bytes for the purposes of
   layout.

   The symbolic bytes abstract away from the details of how
   instructions are represented in memory.  Each instruction takes
   exactly eight consecutive bytes, where the first byte InsB0 stores
   the actual instruction, and the next sevent bytes are InsFrag
   elements, which aren't valid data.

   For example, the two-instruction sequence:
        at&t syntax             ocaml syntax
      movq %rdi, (%rsp)       Movq,  [~%Rdi; Ind2 Rsp]
      decq %rdi               Decq,  [~%Rdi]

   is represented by the following elements of the mem array (starting
   at address 0x400000):

       0x400000 :  InsB0 (Movq,  [~%Rdi; Ind2 Rsp])
       0x400001 :  InsFrag
       0x400002 :  InsFrag
       0x400003 :  InsFrag
       0x400004 :  InsFrag
       0x400005 :  InsFrag
       0x400006 :  InsFrag
       0x400007 :  InsFrag
       0x400008 :  InsB0 (Decq,  [~%Rdi])
       0x40000A :  InsFrag
       0x40000B :  InsFrag
       0x40000C :  InsFrag
       0x40000D :  InsFrag
       0x40000E :  InsFrag
       0x40000F :  InsFrag
       0x400010 :  InsFrag
*)
type sbyte = InsB0 of ins       (* 1st byte of an instruction *)
           | InsFrag            (* 2nd - 7th bytes of an instruction *)
           | Byte of char       (* non-instruction byte *)

(* memory maps addresses to symbolic bytes *)
type mem = sbyte array

(* Flags for condition codes *)
type flags = { mutable fo : bool
             ; mutable fs : bool
             ; mutable fz : bool
             }

(* Register files *)
type regs = int64 array

(* Complete machine state *)
type mach = { flags : flags
            ; regs : regs
            ; mem : mem
            }

(* simulator helper functions ----------------------------------------------- *)

(* The index of a register in the regs array *)
let rind : reg -> int = function
  | Rip -> 16
  | Rax -> 0  | Rbx -> 1  | Rcx -> 2  | Rdx -> 3
  | Rsi -> 4  | Rdi -> 5  | Rbp -> 6  | Rsp -> 7
  | R08 -> 8  | R09 -> 9  | R10 -> 10 | R11 -> 11
  | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15

(* Helper functions for reading/writing sbytes *)

(* Convert an int64 to its sbyte representation *)
let sbytes_of_int64 (i:int64) : sbyte list =
  let open Char in 
  let open Int64 in
  List.map (fun n -> Byte (shift_right i n |> logand 0xffL |> to_int |> chr))
    [0; 8; 16; 24; 32; 40; 48; 56]

(* Convert an sbyte representation to an int64 *)
let int64_of_sbytes (bs:sbyte list) : int64 =
  let open Char in
  let open Int64 in
  let f b i = match b with
    | Byte c -> logor (shift_left i 8) (c |> code |> of_int)
    | _ -> 0L
  in
  List.fold_right f bs 0L

(* Convert a string to its sbyte representation *)
let sbytes_of_string (s:string) : sbyte list =
  let rec loop acc = function
    | i when i < 0 -> acc
    | i -> loop (Byte s.[i]::acc) (pred i)
  in
  loop [Byte '\x00'] @@ String.length s - 1

(* Serialize an instruction to sbytes *)
let sbytes_of_ins (op, args:ins) : sbyte list =
  let check = function
    | Imm (Lbl _) | Ind1 (Lbl _) | Ind3 (Lbl _, _) -> 
      invalid_arg "sbytes_of_ins: tried to serialize a label!"
    | o -> ()
  in
  List.iter check args;
  [InsB0 (op, args); InsFrag; InsFrag; InsFrag; InsFrag; InsFrag; InsFrag; InsFrag]

(* Serialize a data element to sbytes *)
let sbytes_of_data : data -> sbyte list = function
  | Quad (Lit i) -> sbytes_of_int64 i
  | Asciz s -> sbytes_of_string s
  | Quad (Lbl _) -> invalid_arg "sbytes_of_data: tried to serialize a label!"


(* It might be useful to toggle printing of intermediate states of your 
   simulator. *)
let debug_simulator = ref true

(* Interpret a condition code with respect to the given flags. *)
let interp_cnd {fo; fs; fz} : cnd -> bool = 
  let rec interp x = 
    begin match x with
      | Eq -> fz
      | Neq -> not fz
      | Lt -> (fs && not fo) || (not fs && fo)
      | Le -> interp Lt || interp Eq
      | Gt -> not (interp Le)
      | Ge -> not (interp Lt)
    end
  in
  interp

(* Maps an X86lite address into Some OCaml array index,
   or None if the address is not within the legal address space. *)
let map_addr (addr:quad) : int option =
  if addr < mem_bot || addr >= mem_top
  then None
  else Some (Int64.to_int (Int64.sub addr mem_bot));;

let map_addr_fatal (addr:quad) : int =
  begin match map_addr addr with
    | Some x -> x
    | None -> raise X86lite_segfault
  end

let interpret_imm (i:imm) : int64 =
  begin match i with
    | Lit x -> x
    | _ -> failwith "Labels should no exist" (* labels shouldnt exist, i guess *)
  end 

let incr_rip (m:mach) : unit =
  m.regs.(rind Rip) <- (Int64.add m.regs.(rind Rip) ins_size)

let write_mem (value:int64) (addr:int64) (m:mach) : unit =
  Array.blit (Array.of_list (sbytes_of_int64 value)) 0 m.mem (map_addr_fatal addr) 8

let read_mem (addr:int64) (m:mach) : sbyte list = 
  Array.to_list @@ Array.sub m.mem (map_addr_fatal addr) 8

let push_to_stack (value:int64) (m:mach) : unit =
  let next = Int64.sub m.regs.(rind Rsp) ins_size in
  write_mem value next m;
  m.regs.(rind Rsp) <- next

let pop_from_stack (m:mach) : int64 =
  let v = read_mem m.regs.(rind Rsp) m in
  m.regs.(rind Rsp) <- Int64.add m.regs.(rind Rsp) ins_size;
  int64_of_sbytes v

let interpret_mem_loc (op:operand) (m:mach) : int64 =
  begin match op with
    | Imm imm -> failwith "Imm is not a memory location"
    | Reg reg -> failwith "Reg is not a memory location"
    | Ind1 imm -> interpret_imm imm
    | Ind2 reg -> m.regs.(rind reg)
    | Ind3 (imm, reg) -> Int64.add (interpret_imm imm) (m.regs.(rind reg))
  end

let interpret_val (op:operand) (m:mach) : int64 =
  begin match op with
    | Imm imm ->  interpret_imm imm
    | Reg reg -> m.regs.(rind reg)
    | _ -> int64_of_sbytes @@ read_mem (interpret_mem_loc op m) m
  end

let save_res (value:int64) (dest:operand) (m:mach) : unit =
  begin match dest with
    | Imm imm -> failwith "you cannot save to a value :P"
    | Reg reg -> Array.set m.regs (rind reg) value
    | _ -> write_mem value (interpret_mem_loc dest m) m
  end

(* Ind3 (Lit i, Rsp) *)

let set_flags (fo:bool) (value:int64) (m:mach) : unit =
  m.flags.fo <- fo;
  m.flags.fs <- (value < 0L);
  m.flags.fz <- (value = 0L)

let arith_bin_op (operation: int64 -> int64 -> Int64_overflow.t) (src:operand) (dest:operand) (m:mach) : unit =
  let res = operation (interpret_val dest m) (interpret_val src m) in
  set_flags res.overflow res.value m;
  save_res res.value dest m

let arith_un_op (operation: int64 -> Int64_overflow.t) (src:operand) (m:mach) : unit =
  let res = operation (interpret_val src m) in
  set_flags res.overflow res.value m;
  save_res res.value src m

let shift_op (operation: int64 -> int -> int64) (decide_fo: int64 -> int64 -> bool) (amt:operand) (dest:operand) (m:mach) : unit =
  let input = interpret_val dest m in
  let amt_val = Int64.to_int @@ interpret_val amt m in
  let output = operation input amt_val in
  begin match amt_val with
    | 0 -> ()
    | 1 -> set_flags (decide_fo input output) output m
    | _ -> set_flags m.flags.fo output m
  end;
  save_res output dest m

let log_bin_op (operation: int64 -> int64 -> int64) (src:operand) (dest:operand) (m:mach) : unit =
  let res = operation (interpret_val dest m) (interpret_val src m) in
  set_flags false res m;
  save_res res dest m

let log_un_op (operation: int64 -> int64) (src:operand) (m:mach) : unit =
  let res = operation @@ interpret_val src m in
  set_flags false res m;
  save_res res src m

let msb_as_bool (value: int64) : bool =
  let msb = Int64.logand value @@ Int64.lognot Int64.max_int in
  not @@ Int64.equal Int64.zero msb

let interpret_instr_base (instr:ins) (m:mach) : unit =
  begin match instr with
    (* Arithmetic Instructions *)
    | Addq, [src; dest] -> arith_bin_op Int64_overflow.add src dest m
    | Subq, [src; dest] -> arith_bin_op Int64_overflow.sub src dest m
    | Imulq, [src; dest] -> arith_bin_op Int64_overflow.mul src dest m
    | Incq, [src] -> arith_un_op Int64_overflow.succ src m
    | Decq, [src] -> arith_un_op Int64_overflow.pred src m
    | Negq, [src] -> arith_un_op Int64_overflow.neg src m
    (* Logic Instructions *)
    | Andq, [src; dest] -> log_bin_op Int64.logand src dest m
    | Orq, [src; dest] -> log_bin_op Int64.logor src dest m
    | Xorq, [src; dest] -> log_bin_op Int64.logxor src dest m
    | Notq, [src] -> save_res (Int64.lognot @@ interpret_val src m) src m
    (* Bit-manipulation Instructions *)
    | Sarq, [amt; dest] ->
      let decide_fo _ _ = false in
      shift_op Int64.shift_right decide_fo amt dest m
    | Shlq, [amt; dest] -> 
      let decide_fo _ output = (msb_as_bool output) <> (msb_as_bool @@ Int64.shift_left output 1) in
      shift_op Int64.shift_left decide_fo amt dest m
    | Shrq, [amt; dest] ->
      let decide_fo input _ = msb_as_bool input in
      shift_op Int64.shift_right decide_fo amt dest m
    | Set cc, [dest] ->
      let b = if interp_cnd m.flags cc then Int64.one else Int64.zero in
      let mask = Int64.of_int 255 in
      save_res (Int64.logor b (Int64.logand mask @@ interpret_val dest m)) dest m
    (* Data-movement Instructions *)
    | Leaq, [ind; dest] -> save_res (interpret_mem_loc ind m) dest m
    | Movq, [src; dest] -> save_res (interpret_val src m) dest m
    | Pushq, [src] -> push_to_stack (interpret_val src m) m
    | Popq, [dest] -> save_res (pop_from_stack m) dest m
    (* Control-flow and condition Instructions *)
    | Cmpq, [src1; src2] ->
      let res = Int64_overflow.sub (interpret_val src2 m) (interpret_val src1 m) in
      set_flags res.overflow res.value m
    | Callq, [src] ->
      push_to_stack m.regs.(rind Rip) m;
      m.regs.(rind Rip) <- interpret_val src m
    | Retq, [] -> m.regs.(rind Rip) <- pop_from_stack m
    | Jmp, [src] -> m.regs.(rind Rip) <- interpret_val src m
    | J cc, [src] ->
      if interp_cnd m.flags cc
      then m.regs.(rind Rip) <- interpret_val src m
      else incr_rip m
    | _ -> failwith "this instruction should not exist"
  end

let interpret_instr (instr:ins) (m:mach) : unit =
  incr_rip m;
  interpret_instr_base instr m

let get_instr (m:mach) : ins =
  let addr = map_addr @@ m.regs.(rind Rip) in
  begin match addr with
    | Some a -> begin match Array.get m.mem a with
        | InsB0 b -> b
        | _ -> raise X86lite_segfault (* TODO: what else ? *)
      end
    | None -> raise X86lite_segfault
  end

(* Simulates one step of the machine:
   - fetch the instruction at %rip
   - compute the source and/or destination information from the operands
   - simulate the instruction semantics
   - update the registers and/or memory appropriately
   - set the condition flags
*)
let step (m:mach) : unit =
  let instr = get_instr m in 
  interpret_instr instr m

(* Runs the machine until the rip register reaches a designated
   memory address. *)
let run (m:mach) : int64 = 
  while m.regs.(rind Rip) <> exit_addr do step m done;
  m.regs.(rind Rax)

(* assembling and linking --------------------------------------------------- *)

(* A representation of the executable *)
type exec = { entry    : quad              (* address of the entry point *)
            ; text_pos : quad              (* starting address of the code *)
            ; data_pos : quad              (* starting address of the data *)
            ; text_seg : sbyte list        (* contents of the text segment *)
            ; data_seg : sbyte list        (* contents of the data segment *)
            }

(* Assemble should raise this when a label is used but not defined *)
exception Undefined_sym of lbl

(* Assemble should raise this when a label is defined more than once *)
exception Redefined_sym of lbl

(* replace_instr_labels replaces all references to labels in an instruction with the concrete location *)
let replace_instr_labels (sym_loc: string -> int64) ((opcode, operands):ins) : ins =
  let map_imm old = begin match old with
    | Lit _ -> old
    | Lbl l -> Lit (sym_loc l)
  end in
  let map_operand o = begin match o with
    | Imm imm -> Imm (map_imm imm)
    | Ind1 imm -> Ind1 (map_imm imm)
    | Ind3 (imm, reg) -> Ind3 (map_imm imm, reg)
    | _ -> o
  end in
  (opcode, List.map map_operand operands)

let instr_to_sbytes (sym_loc: string -> int64) (instr:ins)  : sbyte list = 
  [InsB0 (replace_instr_labels sym_loc instr); InsFrag; InsFrag; InsFrag; InsFrag; InsFrag; InsFrag; InsFrag] 

let data_to_sbytes (sym_loc: string -> int64) (d:data) : sbyte list = 
  begin match d with
    | Asciz s ->
      let base = List.init (String.length s) (fun i -> Byte (String.get s i)) in
      List.concat [base; [Byte '\x00']]
    | Quad (Lit v) -> sbytes_of_int64 v
    | Quad (Lbl l) -> sbytes_of_int64 @@ sym_loc l
  end

let asm_to_sbytes (sym_loc: string -> int64) (a:asm)  : sbyte list =
  begin match a with
    | Text instructions -> instructions
                           |> List.map @@ instr_to_sbytes sym_loc
                           |> List.flatten
    | Data parts -> parts
                    |> List.map @@ data_to_sbytes sym_loc
                    |> List.flatten
  end

(* asm_size deterimes the size of an assembly section in bytes *)
let asm_size (a:asm) : int = List.length @@ asm_to_sbytes (fun _ -> 0L) a

(* p must be sorted before calling get_symbol_location *)
let make_get_symbol_location (p:prog) : string -> int64 = 
  let locs_by_name = Hashtbl.create @@ List.length p in
  let cur_pos = ref mem_bot in
  List.iter (fun e ->
      Hashtbl.add locs_by_name e.lbl !cur_pos;
      cur_pos := Int64.add !cur_pos @@ Int64.of_int @@ asm_size e.asm
    ) p;
  let get_symbol_location name = begin match Hashtbl.find_opt locs_by_name name with
    | Some loc -> loc
    | None -> raise (Undefined_sym name)
  end in
  get_symbol_location

(* Convert an X86 program into an object file:
   - separate the text and data segments
   - compute the size of each segment
      Note: the size of an Asciz string section is (1 + the string length)

   - resolve the labels to concrete addresses and 'patch' the instructions to 
     replace Lbl values with the corresponding Imm values.

   - the text segment starts at the lowest address
   - the data segment starts after the text segment

   HINT: List.fold_left and List.fold_right are your friends.
*)
let assemble (unsorted_p:prog) : exec =
  let is_text (e:elem) = begin match e.asm with
    | Text _ -> true
    | Data _ -> false
  end in
  let (text, data) = List.partition is_text unsorted_p in
  let sorted_p = List.concat [text; data] in
  let sym_loc = make_get_symbol_location sorted_p in
  let section_to_sbytes (elems: X86.elem list) = elems
                                                 |> List.map (fun e -> e.asm)
                                                 |> List.map (asm_to_sbytes sym_loc)
                                                 |> List.flatten in
  let text_seg = section_to_sbytes text in
  let text_pos = mem_bot in
  {
    entry = sym_loc "main"; 
    text_pos = text_pos;
    data_pos = Int64.add text_pos @@ Int64.of_int @@ List.length text_seg;
    text_seg = text_seg;
    data_seg = section_to_sbytes data;
  }

(* Convert an object file into an executable machine state. 
   - allocate the mem array
   - set up the memory state by writing the symbolic bytes to the 
      appropriate locations 
   - create the inital register state
   - initialize rip to the entry point address
   - initializes rsp to the last word in memory 
   - the other registers are initialized to 0
   - the condition code flags start as 'false'

   Hint: The Array.make, Array.blit, and Array.of_list library functions 
   may be of use.
*)
let load {entry; text_pos; data_pos; text_seg; data_seg} : mach = 
  let mem = (Array.make mem_size (Byte '\x00')) in
  Array.blit (Array.of_list text_seg) 0 mem (Int64.to_int (Int64.sub text_pos mem_bot)) (List.length text_seg);
  Array.blit (Array.of_list data_seg) 0 mem (Int64.to_int (Int64.sub data_pos mem_bot)) (List.length data_seg);
  Array.blit (Array.of_list (sbytes_of_int64 exit_addr)) 0 mem (mem_size-8) 8;
  let regs = Array.make nregs 0L in
  regs.(rind Rip) <- entry;
  regs.(rind Rsp) <- Int64.sub mem_top 8L;
  { flags = {fo = false; fs = false; fz = false};
    regs = regs;
    mem = mem
  }