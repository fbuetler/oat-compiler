(* ll ir compilation -------------------------------------------------------- *)

open Ll
open X86
open Asm

(* TODO check that we are following calling conventions everywhere (eg. callee-saved vs caller-saved registers) *)

(* Overview ----------------------------------------------------------------- *)

(* We suggest that you spend some time understinging this entire file and 
   how it fits with the compiler pipeline before making changes.  The suggested
   plan for implementing the compiler is provided on the project web page. 
*)


(* helpers ------------------------------------------------------------------ *)

(* Map LL comparison operations to X86 condition codes *)
let compile_cnd = function
  | Ll.Eq  -> X86.Eq
  | Ll.Ne  -> X86.Neq
  | Ll.Slt -> X86.Lt
  | Ll.Sle -> X86.Le
  | Ll.Sgt -> X86.Gt
  | Ll.Sge -> X86.Ge

(* Defines sets of strings *)
module SS = Set.Make(String)

let rec drop (n: int) (l: 'a list) : 'a list =
  begin match l with
    | [] -> []
    | h::t -> if n > 0 then drop (n-1) t else h::t
  end

let rec take (n: int) (l: 'a list) : 'a list =
  begin match l with
    | [] -> []
    | h::t -> if n > 0 then h::(take (n-1) t) else []
  end

(* locals and layout -------------------------------------------------------- *)

(* One key problem in compiling the LLVM IR is how to map its local
   identifiers to X86 abstractions.  For the best performance, one
   would want to use an X86 register for each LLVM %uid.  However,
   since there are an unlimited number of %uids and only 16 registers,
   doing so effectively is quite difficult.  We will see later in the
   course how _register allocation_ algorithms can do a good job at
   this.

   A simpler, but less performant, implementation is to map each %uid
   in the LLVM source to a _stack slot_ (i.e. a region of memory in
   the stack).  Since LLVMlite, unlike real LLVM, permits %uid locals
   to store only 64-bit data, each stack slot is an 8-byte value.

   [ NOTE: For compiling LLVMlite, even i1 data values should be
   represented as a 8-byte quad. This greatly simplifies code
   generation. ]

   We call the datastructure that maps each %uid to its stack slot a
   'stack layout'.  A stack layout maps a uid to an X86 operand for
   accessing its contents.  For this compilation strategy, the operand
   is always an offset from ebp (in bytes) that represents a storage slot in
   the stack.  
*)

type layout = (uid * X86.operand) list

(* A context contains the global type declarations (needed for getelementptr
   calculations) and a stack layout. *)
type ctxt = { tdecls : (tid * ty) list
            ; layout : layout
            ; f_name : string
            ; f_lbl_max_length : int
            }

(* useful for looking up items in tdecls or layouts *)
let lookup m x = List.assoc x m

let rec pad_string (padding:string) (len:int) (s:string): string =
  if len > String.length s 
  then pad_string padding len (s^padding)
  else s

let lbl_for_asm (ctxt:ctxt) (lbl:lbl) : X86.lbl = 
  Platform.mangle @@
  pad_string "_" ctxt.f_lbl_max_length ctxt.f_name ^"_"^ lbl

(* compiling operands  ------------------------------------------------------ *)

(* LLVM IR instructions support several kinds of operands.

   LL local %uids live in stack slots, whereas global ids live at
   global addresses that must be computed from a label.  Constants are
   immediately available, and the operand Null is the 64-bit 0 value.

     NOTE: two important facts about global identifiers:

     (1) You should use (Platform.mangle gid) to obtain a string 
     suitable for naming a global label on your platform (OS X expects
     "_main" while linux expects "main").

     (2) 64-bit assembly labels are not allowed as immediate operands.
     That is, the X86 code: movq _gid %rax which looks like it should
     put the address denoted by _gid into %rax is not allowed.
     Instead, you need to compute an %rip-relative address using the
     leaq instruction:   leaq _gid(%rip).

   One strategy for compiling instruction operands is to use a
   designated register (or registers) for holding the values being
   manipulated by the LLVM IR instruction. You might find it useful to
   implement the following helper function, whose job is to generate
   the X86 instruction that moves an LLVM operand into a designated
   destination (usually a register).  
*)
let compile_operand ctxt (dest: X86.operand) : Ll.operand -> ins =
  function op -> 
    begin match op with
      | Null -> Movq, [~$0; dest]
      | Const v -> Movq, [Imm (Lit v); dest]
      | Id id -> Movq, [lookup ctxt.layout id; dest]
      | Gid gid -> Leaq, [Ind3(Lbl (Platform.mangle gid), Rip); dest]
    end



(* compiling call  ---------------------------------------------------------- *)

(* You will probably find it helpful to implement a helper function that 
   generates code for the LLVM IR call instruction.

   The code you generate should follow the x64 System V AMD64 ABI
   calling conventions, which places the first six 64-bit (or smaller)
   values in registers and pushes the rest onto the stack.  Note that,
   since all LLVM IR operands are 64-bit values, the first six
   operands will always be placed in registers.  (See the notes about
   compiling fdecl below.)

   [ NOTE: It is the caller's responsibility to clean up arguments
   pushed onto the stack, so you must free the stack space after the
   call returns. ]

   [ NOTE: Don't forget to preserve caller-save registers (only if
   needed). ]
*)
let compile_call ctxt fop args =
  failwith "compile_call not implemented"



(* compiling getelementptr (gep)  ------------------------------------------- *)

(* The getelementptr instruction computes an address by indexing into
   a datastructure, following a path of offsets.  It computes the
   address based on the size of the data, which is dictated by the
   data's type.

   To compile getelmentptr, you must generate x86 code that performs
   the appropriate arithemetic calculations.
*)

(* [size_ty] maps an LLVMlite type to a size in bytes. 
    (needed for getelementptr)

   - the size of a struct is the sum of the sizes of each component
   - the size of an array of t's with n elements is n * the size of t
   - all pointers, I1, and I64 are 8 bytes
   - the size of a named type is the size of its definition

   - Void, i8, and functions have undefined sizes according to LLVMlite.
     Your function should simply return 0 in those cases
*)
let rec size_ty tdecls t : int =
  let size = size_ty tdecls in
  begin match t with
    | Void -> 0
    | I1 -> 8
    | I8 -> 0
    | I64 -> 8
    | Ptr (ty) -> 8
    | Struct (ty) -> ty |> List.map size |> List.fold_left (+) 0
    | Array (len, ty) -> len * size ty
    | Fun (_, _) -> 0
    | Namedt (name) -> size @@ lookup tdecls name
  end


let rec gep_helper (ctxt:ctxt) (op_ty: Ll.ty) (path: int option list) : (int * int) list =
  let size = size_ty ctxt.tdecls in
  begin match path with
    | h::tail -> 
      let recurse next_ty = gep_helper ctxt next_ty tail in
      begin match op_ty with
        | Namedt tid -> gep_helper ctxt (lookup ctxt.tdecls tid) path
        | Ptr ty -> (size ty, 0)::(recurse ty)
        | Array (_, ty) -> (size ty, 0)::(recurse ty)
        | Struct tys -> 
          let h_const = begin match h with
            | Some v -> v
            | None -> failwith "index operand must be a constant when indexing into struct"
          end in
          let offset = List.fold_left (fun sum ty -> sum + size ty) 0 @@ take h_const tys in
          (0, offset)::(recurse @@ List.nth tys h_const)
        | _ -> failwith "gep: unsupported type"
      end
    | [] -> []
  end

(* Generates code that computes a pointer value.  

   1. op must be of pointer type: t*

   2. the value of op is the base address of the calculation

   3. the first index in the path is treated as the index into an array
     of elements of type t located at the base address

   4. subsequent indices are interpreted according to the type t:

   - if t is a struct, the index must be a constant n and it 
       picks out the n'th element of the struct. [ NOTE: the offset
       within the struct of the n'th element is determined by the 
       sizes of the types of the previous elements ]

   - if t is an array, the index can be any operand, and its
       value determines the offset within the array.

   - if t is any other type, the path is invalid

   5. if the index is valid, the remainder of the path is computed as
      in (4), but relative to the type f the sub-element picked out
      by the path so far
*)
let rec compile_gep (ctxt:ctxt) (op : Ll.ty * X86.operand) (ll_path: Ll.operand list) (asm_path: X86.ins list) : ins list =
  let op_to_const el = begin match el with
    | Const v -> Some (Int64.to_int v)
    | _ -> None
  end in
  let factors = gep_helper ctxt (fst op) (List.map op_to_const ll_path) in
  List.concat @@ List.map2 (fun (factor, offset) load -> [
        load;
        Imulq, [~$factor; ~%Rcx];
        Addq, [~$offset; ~%Rcx];
        Addq, [~%Rcx; snd op];
      ]) factors asm_path

let ll_bop_to_opcode (bop: Ll.bop) : X86.opcode = begin match bop with
  | Add -> Addq
  | Sub -> Subq
  | Mul -> Imulq
  | Shl -> Shlq
  | Lshr -> Shrq
  | Ashr -> Sarq
  | And -> Andq
  | Or -> Orq
  | Xor -> Xorq
end

let ll_cnd_to_asm (cnd: Ll.cnd) : X86.cnd = begin match cnd with
  | Eq -> Eq
  | Ne -> Neq
  | Slt -> Lt
  | Sle -> Le
  | Sgt -> Gt
  | Sge -> Ge
end

(* This helper function computes the location of the nth incoming
   function argument: either in a register or relative to %rbp,
   according to the calling conventions.  You might find it useful for
   compile_fdecl.

   [ NOTE: the first six arguments are numbered 0 .. 5 ]
*)
let arg_loc_base (base: reg) (n : int) : operand =
  let regs = [Rdi; Rsi; Rdx; Rcx; R08; R09] in
  begin match List.nth_opt regs n with
    | Some v -> Reg v
    | None -> Ind3 (Lit (Int64.of_int @@ 8 * (n-6+2)), base)
    (* -6 since the 6th argument is the first one to be passed on the stack instead of in registers *)
    (* +2 since we ignore "saved RBP" and "return address" https://eli.thegreenplace.net/images/2011/08/x64_frame_nonleaf.png *)
  end

let arg_loc = arg_loc_base Rbp

(* compiling instructions  -------------------------------------------------- *)

(* The result of compiling a single LLVM instruction might be many x86
   instructions.  We have not determined the structure of this code
   for you. Some of the instructions require only a couple assembly
   instructions, while others require more.  We have suggested that
   you need at least compile_operand, compile_call, and compile_gep
   helpers; you may introduce more as you see fit.

   Here are a few notes:

   - Icmp:  the Set instruction may be of use.  Depending on how you
     compile Cbr, you may want to ensure that the value produced by
     Icmp is exactly 0 or 1.

   - Load & Store: these need to dereference the pointers. Const and
     Null operands aren't valid pointers.  Don't forget to
     Platform.mangle the global identifier.

   - Alloca: needs to return a pointer into the stack

   - Bitcast: does nothing interesting at the assembly level
*)
let compile_insn ctxt (uid, i) : X86.ins list =
  let comp_op = compile_operand ctxt in
  let dest = lookup ctxt.layout uid in
  begin match i with
    | Binop (bop, _, a, b) -> [
        comp_op ~%Rcx b;
        comp_op ~%Rax a;
        ll_bop_to_opcode bop, [~%Rcx; ~%Rax];
        Movq, [~%Rax; lookup ctxt.layout uid]
      ]
    | Icmp (cnd, _, a, b) -> [
        comp_op ~%Rcx b;
        comp_op ~%Rax a;
        Cmpq, [~%Rcx; ~%Rax];
        Movq, [~$0; dest];
        Set (ll_cnd_to_asm cnd), [dest];
      ]
    | Alloca ty -> [
        Subq, [~$(size_ty ctxt.tdecls ty); ~%Rsp];
        Movq, [~%Rsp; dest];
      ]
    | Store (ty, src, dst) -> [
        comp_op ~%Rcx src;
        comp_op ~%Rax dst;
        Movq, [~%Rcx; Ind2 Rax];
      ]
    | Load (ty, src) -> [
        comp_op ~%Rcx src;
        Movq, [Ind2 Rcx; ~%Rax];
        Movq, [~%Rax; dest];
      ]
    | Call (_, f, args) -> 
      (* TODO support call with void return *)
      List.concat [
        List.map (fun _ -> Pushq, [~$0]) @@ drop 6 args;
        [
          Leaq, [Ind3 (Lit (-16L), Rsp); ~%R10];
        ];
        List.concat @@ List.mapi (fun i (_, operand) -> [
              comp_op ~%Rax operand;
              Movq, [~%Rax; arg_loc_base R10 i];
            ]) args;
        [
          comp_op ~%Rax f;
          Callq, [~%Rax];
          Movq, [~%Rax; dest];
        ];
        List.map (fun _ -> Popq, [~%Rax]) @@ drop 6 args;
      ]
    | Bitcast (_, operand, _) -> [
        comp_op ~%Rax operand;
        Movq, [~%Rax; dest];
      ]
    | Gep (op_ty, op_v, path) -> List.concat [
        [comp_op ~%Rax op_v];
        compile_gep ctxt (op_ty, ~%Rax) path @@ List.map (comp_op ~%Rcx) path;
        [Movq, [~%Rax; dest]];
      ]
  end



(* compiling terminators  --------------------------------------------------- *)

(* Compile block terminators is not too difficult:

   - Ret should properly exit the function: freeing stack space,
     restoring the value of %rbp, and putting the return value (if
     any) in %rax.

   - Br should jump

   - Cbr branch should treat its operand as a boolean conditional
*)
let compile_terminator ctxt t: ins list =
  begin match t with
    | Ret (_, operand) -> [
        compile_operand ctxt ~%Rax (
          begin match operand with
            | Some v -> v
            | None -> Const 0L
          end
        );
        (* based on the following line from http://tldp.org/LDP/LG/issue94/ramankutty.html*)
        (* "movl %ebp, %esp; popl %ebp ret" *)
        Movq, [~%Rbp; ~%Rsp];
        Popq, [~%Rbp];
        Retq, [];
      ]
    | Br lbl -> [
        Jmp, [~$$(lbl_for_asm ctxt lbl)];
      ]
    | Cbr (operand, lbl_t, lbl_f) -> [
        compile_operand ctxt ~%Rax operand;
        Cmpq, [~$0; ~%Rax];
        J Neq, [~$$(lbl_for_asm ctxt lbl_t)];
        J Eq, [~$$(lbl_for_asm ctxt lbl_f)];
      ]
  end


(* compiling blocks --------------------------------------------------------- *)

(* We have left this helper function here for you to complete. *)
let compile_block ctxt blk : ins list =
  (List.flatten @@ List.map (compile_insn ctxt) blk.insns)
  @ compile_terminator ctxt @@ snd blk.term

let compile_lbl_block lbl ctxt blk : elem =
  Asm.text lbl (compile_block ctxt blk)



(* compile_fdecl ------------------------------------------------------------ *)


(* ids_from_block returns a set of all uids assigned to in a block*)
let ids_from_block (b: block) : SS.t =
  List.fold_left
    (fun set el -> SS.add el set)
    SS.empty
    ((fst b.term)::(List.map fst b.insns))

(* We suggest that you create a helper function that computes the 
   stack layout for a given function declaration.

   - each function argument should be copied into a stack slot
   - in this (inefficient) compilation strategy, each local id 
     is also stored as a stack slot.
   - see the discusion about locals 

*)
let stack_layout (args: Ll.uid list) ((block, lbled_blocks): cfg) : layout =
  let blocks = block::(List.map snd lbled_blocks) in
  let ids_from_blocks = List.fold_left (fun set b -> SS.union set @@ ids_from_block b) SS.empty blocks in
  let ids = List.sort String.compare @@ (args @ (SS.elements ids_from_blocks)) in
  List.mapi (fun i id -> (id, Ind3 (Lit (Int64.of_int @@ -8 * (i + 1)), Rbp))) ids

(* The code for the entry-point of a function must do several things:

   - since our simple compiler maps local %uids to stack slots,
     compiling the control-flow-graph body of an fdecl requires us to
     compute the layout (see the discussion of locals and layout)

   - the function code should also comply with the calling
     conventions, typically by moving arguments out of the parameter
     registers (or stack slots) into local storage space.  For our
     simple compilation strategy, that local storage space should be
     in the stack. (So the function parameters can also be accounted
     for in the layout.)

   - the function entry code should allocate the stack storage needed
     to hold all of the local stack slots.
*)
let compile_fdecl tdecls f_lbl_max_length name { f_ty; f_param; f_cfg } : X86.prog =
  let layout = stack_layout f_param f_cfg in 
  let ctxt: ctxt = {
    tdecls = tdecls;
    layout = layout;
    f_name = name;
    f_lbl_max_length = f_lbl_max_length;
  } in
  let initialization_asm: ins list = [ (* based on section 9 of http://tldp.org/LDP/LG/issue94/ramankutty.html *)
    (* save the old base pointer, so we can restore it when returnig *)
    Pushq, [~%Rbp];
    (* our stack starts where the old one ended *)
    Movq, [~%Rsp; ~%Rbp];
    (* make space for everything we will have on the stack (assumes layout has no empty space between items) *)
    Subq, [~$(8 * List.length layout); ~%Rsp];
  ] in
  let copy_vars_asm =
    f_param
    |> List.mapi (fun i p -> [
          Movq, [arg_loc_base Rbp i; ~%Rax];
          Movq, [~%Rax; lookup layout p];
        ])
    |> List.concat
  in
  [
    {
      lbl = Platform.mangle name;
      global = true;
      asm = Text (List.concat [
          initialization_asm;
          copy_vars_asm;
          compile_block ctxt @@ fst f_cfg;
        ]);
    };
  ] @ List.map (
    fun (lbl, block) -> compile_lbl_block (lbl_for_asm ctxt lbl) ctxt block
  ) @@ snd f_cfg



(* compile_gdecl ------------------------------------------------------------ *)
(* Compile a global value into an X86 global data declaration and map
   a global uid to its associated X86 label.
*)
let rec compile_ginit = function
  | GNull     -> [Quad (Lit 0L)]
  | GGid gid  -> [Quad (Lbl (Platform.mangle gid))]
  | GInt c    -> [Quad (Lit c)]
  | GString s -> [Asciz s]
  | GArray gs | GStruct gs -> List.map compile_gdecl gs |> List.flatten

and compile_gdecl (_, g) = compile_ginit g


(* compile_prog ------------------------------------------------------------- *)
let compile_prog {tdecls; gdecls; fdecls} : X86.prog =
  let g = fun (lbl, gdecl) -> Asm.data (Platform.mangle lbl) (compile_gdecl gdecl) in
  let philippe_likes_it_correct = List.fold_left max 0 @@ List.map (fun s -> String.length (fst s)) fdecls in
  let f = fun (name, fdecl) -> compile_fdecl tdecls philippe_likes_it_correct name fdecl in
  (List.map g gdecls) @ (List.map f fdecls |> List.flatten)
