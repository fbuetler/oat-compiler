open Ll
open Llutil
open Ast

(* instruction streams ------------------------------------------------------ *)

(* As in the last project, we'll be working with a flattened representation
   of LLVMlite programs to make emitting code easier. This version
   additionally makes it possible to emit elements will be gathered up and
   "hoisted" to specific parts of the constructed CFG
   - G of gid * Ll.gdecl: allows you to output global definitions in the middle
     of the instruction stream. You will find this useful for compiling string
     literals
   - E of uid * insn: allows you to emit an instruction that will be moved up
     to the entry block of the current function. This will be useful for 
     compiling local variable declarations
*)

type elt = 
  | L of Ll.lbl             (* block labels *)
  | I of uid * Ll.insn      (* instruction *)
  | T of Ll.terminator      (* block terminators *)
  | G of gid * Ll.gdecl     (* hoisted globals (usually strings) *)
  | E of uid * Ll.insn      (* hoisted entry block instructions *)

type stream = elt list
let ( >@ ) x y = y @ x
let ( >:: ) x y = y :: x
let lift : (uid * insn) list -> stream = List.rev_map (fun (x,i) -> I (x,i))

(* Build a CFG and collection of global variable definitions from a stream *)
let cfg_of_stream (code:stream) : Ll.cfg * (Ll.gid * Ll.gdecl) list  =
  let gs, einsns, insns, term_opt, blks = List.fold_left
      (fun (gs, einsns, insns, term_opt, blks) e ->
         match e with
         | L l ->
           begin match term_opt with
             | None -> 
               if (List.length insns) = 0 then (gs, einsns, [], None, blks)
               else failwith @@ Printf.sprintf "build_cfg: block labeled %s has\
                                                no terminator" l
             | Some term ->
               (gs, einsns, [], None, (l, {insns; term})::blks)
           end
         | T t  -> (gs, einsns, [], Some (Llutil.Parsing.gensym "tmn", t), blks)
         | I (uid,insn)  -> (gs, einsns, (uid,insn)::insns, term_opt, blks)
         | G (gid,gdecl) ->  ((gid,gdecl)::gs, einsns, insns, term_opt, blks)
         | E (uid,i) -> (gs, (uid, i)::einsns, insns, term_opt, blks)
      ) ([], [], [], None, []) code
  in
  match term_opt with
  | None -> failwith "build_cfg: entry block has no terminator" 
  | Some term -> 
    let insns = einsns @ insns in
    ({insns; term}, blks), gs


(* compilation contexts ----------------------------------------------------- *)

(* To compile OAT variables, we maintain a mapping of source identifiers to the
   corresponding LLVMlite operands. Bindings are added for global OAT variables
   and local variables that are in scope. *)

module Ctxt = struct

  type t = (Ast.id * (Ll.ty * Ll.operand)) list
  let empty = []

  (* Add a binding to the context *)
  let add (c:t) (id:id) (bnd:Ll.ty * Ll.operand) : t = (id,bnd)::c

  (* Lookup a binding in the context *)
  let lookup (id:Ast.id) (c:t) : Ll.ty * Ll.operand =
    List.assoc id c

  (* Lookup a function, fail otherwise *)
  let lookup_function (id:Ast.id) (c:t) : Ll.ty * Ll.operand =
    match List.assoc id c with
    | Ptr (Fun (args, ret)), g -> Ptr (Fun (args, ret)), g
    | _ -> failwith @@ id ^ " not bound to a function"

  let lookup_function_option (id:Ast.id) (c:t) : (Ll.ty * Ll.operand) option =
    try Some (lookup_function id c) with _ -> None

end

(* compiling OAT types ------------------------------------------------------ *)

(* The mapping of source types onto LLVMlite is straightforward. Booleans and ints
   are represented as the the corresponding integer types. OAT strings are 
   pointers to bytes (I8). Arrays are the most interesting type: they are
   represented as pointers to structs where the first component is the number
   of elements in the following array.

   The trickiest part of this project will be satisfying LLVM's rudimentary type
   system. Recall that global arrays in LLVMlite need to be declared with their
   length in the type to statically allocate the right amount of memory. The 
   global strings and arrays you emit will therefore have a more specific type
   annotation than the output of cmp_rty. You will have to carefully bitcast
   gids to satisfy the LLVM type checker.
*)

let rec cmp_ty : Ast.ty -> Ll.ty = function
  | Ast.TBool  -> I1
  | Ast.TInt   -> I64
  | Ast.TRef r -> Ptr (cmp_rty r)

and cmp_rty : Ast.rty -> Ll.ty = function
  | Ast.RString  -> I8
  | Ast.RArray u -> Struct [I64; Array(0, cmp_ty u)]
  | Ast.RFun (ts, t) -> 
    let args, ret = cmp_fty (ts, t) in
    Fun (args, ret)

and cmp_ret_ty : Ast.ret_ty -> Ll.ty = function
  | Ast.RetVoid  -> Void
  | Ast.RetVal t -> cmp_ty t

and cmp_fty (ts, r) : Ll.fty =
  List.map cmp_ty ts, cmp_ret_ty r


let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Eq | Neq | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)

let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)


(* Some useful helper functions *)

(* Generate a fresh temporary identifier. Since OAT identifiers cannot begin
   with an underscore, these should not clash with any source variables *)
let gensym : string -> string =
  let c = ref 0 in
  fun (s:string) -> incr c; Printf.sprintf "_%s%d" s (!c)

(* Amount of space an Oat type takes when stored in the satck, in bytes.  
   Note that since structured values are manipulated by reference, all
   Oat values take 8 bytes on the stack.
*)
let size_oat_ty (t : Ast.ty) = 8L

(* garr_ty_to_local converts the types of global to their local equivalents.
   It makes all arrays are zero-length and converts strings from arrays to pointers. *)
let rec garr_ty_to_local (ty:Ll.ty) : Ll.ty = begin match ty with
  | Ptr (Array (_, I8)) -> Ptr I8
  | Ptr x -> Ptr (garr_ty_to_local x)
  | Struct x -> Struct (List.map garr_ty_to_local x)
  | Array (_, x) -> Array (0, garr_ty_to_local x)
  | Fun (args, r) -> Fun (List.map garr_ty_to_local args, garr_ty_to_local r)
  | _ -> ty
end

(* Compiles an expression exp in context c, outputting the Ll operand that will
   recieve the value of the expression, and the stream of instructions
   implementing the expression. 

   Tips:
   - use the provided cmp_ty function!

   - string literals (CStr s) should be hoisted. You'll need to bitcast the 
     resulting gid to (Ptr I8)

   - use the provided "oat_alloc_array" function to implement literal arrays
     (CArr) and the (NewArr) expressions

   - we found it useful to write a helper function 
     cmp_exp_as : Ctxt.t -> Ast.exp node -> Ll.ty -> Ll.operand * stream
     that compiles an expression and optionally inserts a bitcast to the
     desired Ll type. This is useful for dealing with OAT identifiers that
     correspond to gids that don't quite have the type you want

*)
let rec cmp_exp (c:Ctxt.t) (exp:Ast.exp node) : Ll.ty * Ll.operand * stream =
  let eval_exprs exprs = 
    let ops, stream = List.fold_left (fun (cur_ops, cur_stream) arg -> 
        let ty, op, stream = cmp_exp c arg in
        ((ty, op) :: cur_ops, stream @ cur_stream)
      ) ([], []) exprs in
    (List.rev ops, stream) in
  let load ty op : Ll.ty * Ll.operand * stream =
    let value = gensym "value" in
    begin match ty with
      | Ptr inner_ty -> (inner_ty, Id value, [I (value, Load (ty, op))])
      | Array (_, I8) -> (Ptr I8, Id value, [I (value, Bitcast (Ptr ty, op, Ptr I8))])
      | Struct _ -> 
        let z_ty = garr_ty_to_local ty in
        (Ptr z_ty, Id value, [I (value, Bitcast (Ptr ty, op, Ptr z_ty))])
      | _ -> failwith "expected pointer or array (global string)"
    end in
  begin match exp.elt with
    | CNull ty ->
      let value = gensym "value" in
      let ptr = gensym "ptr" in
      let ptr_ptr = gensym "ptr_ptr" in
      (cmp_ty ty, Id ptr, [
          I (ptr, Load (Ptr (cmp_ty ty), Id ptr_ptr));
          I (ptr_ptr, Bitcast (Ptr I64, Id value, Ptr (cmp_ty ty)));
          I ("", Store (I64, Const 0L, Id value));
          E (value, Alloca I64);
        ])
    | CBool b -> (I1, Const (if b then 1L else 0L), [])
    | CInt i -> (I64, Const i, [])
    | CStr s -> 
      let arr_name = gensym "string_arr" in
      let arr_ty = Array (String.length s + 1, I8) in
      let local_name = gensym "string_ptr" in
      let stream = [
        I (local_name, Bitcast (Ptr arr_ty, Gid arr_name, Ptr I8));
        G (arr_name, (arr_ty, GString s));
      ] in
      (Ptr I8, (Id local_name), stream)
    | CArr (ty, expl) ->
      let arr_name = gensym "arr" in
      let arr_id = no_loc (Id arr_name) in
      let new_exp = no_loc (NewArr (ty, no_loc (CInt (Int64.of_int @@ List.length expl)))) in
      let created_c, create_stream = cmp_stmt c Void (no_loc (Decl (arr_name, new_exp))) in
      let a_ty, a_op, a_stream = cmp_exp created_c arr_id in
      let block_stream = cmp_block created_c Void (List.mapi (fun i e -> 
          no_loc (Assn (no_loc (Index (arr_id, no_loc (CInt (Int64.of_int i)))), e))
        ) expl) in
      (a_ty, a_op, a_stream @ block_stream @ create_stream)
    | NewArr (ty, exp) ->
      (* TODO Should the elements be zero-initialized? *)
      let _, s_op, s_stream = cmp_exp c exp in
      let a_ty, a_op, a_stream = oat_alloc_array ty s_op in
      (a_ty, a_op, a_stream @ s_stream)
    | Id id -> 
      let ty, op = (Ctxt.lookup id c) in
      begin match op with
        | Id uid -> load ty op
        | Gid gid -> load ty op
        | _ -> (ty, op, [])
      end
    | Index _ ->
      let lhs_ty, lhs_op, lhs_stream = cmp_lhs c exp in
      let load_ty, load_op, load_stream = load lhs_ty lhs_op in
      (load_ty, load_op, load_stream @ lhs_stream)
    | Call (f_exp, args) -> 
      let f_name = begin match f_exp.elt with
        | Id x -> x
        | _ -> failwith "calling a non-identifier expression as a function is not supported"
      end in
      let ret_ty = begin match Ctxt.lookup f_name c with
        | Ptr (Fun (args, ret)), _ -> ret
        | _ -> failwith @@ f_name ^ " not bound to a function"
      end in
      let retval = gensym "retval" in
      let arg_ops, arg_stream = eval_exprs args in
      (ret_ty, Id retval, [I (retval, Call (ret_ty, Gid f_name, arg_ops))] @ arg_stream)
    | Bop (op, left, right) -> bin_op c op left right
    | Uop (op, left) -> un_op c op left
  end

and bin_op (c:Ctxt.t) (op: Ast.binop) (left: Ast.exp node) (right: Ast.exp node) :  Ll.ty * Ll.operand * stream  =
  let left_ty, left_op, left_stream = cmp_exp c left in
  let right_ty, right_op, right_stream = cmp_exp c right in
  let result = gensym "result" in
  let b (opcode: Ll.bop):  Ll.ty * Ll.operand * stream  = 
    (I64, Id result, [I (result, Binop(opcode, I64, left_op, right_op))] @ right_stream @ left_stream) in
  let l (opcode: Ll.bop):  Ll.ty * Ll.operand * stream  = 
    (I1, Id result, [I (result, Binop(opcode, I1, left_op, right_op))] @ right_stream @ left_stream) in
  let c (opcode: Ll.cnd): Ll.ty * Ll.operand * stream = 
    (I1, Id result, [I (result, Icmp(opcode, I64, left_op, right_op))] @ right_stream @ left_stream) in
  begin match op with 
    | Add -> b Add
    | Sub -> b Sub
    | Mul -> b Mul
    | Eq -> c Eq
    | Neq -> c Ne
    | Lt -> c Slt
    | Lte -> c Sle
    | Gt -> c Sgt
    | Gte -> c Sge
    | And -> l And
    | Or -> l Or
    | IAnd -> b And
    | IOr -> b Or
    | Shl -> b Shl
    | Shr -> b Lshr
    | Sar -> b Ashr
  end

and un_op (c:Ctxt.t) (op: Ast.unop) (left: Ast.exp node) : Ll.ty * Ll.operand * stream =
  let inner_ty, inner_op, stream = cmp_exp c left in
  let result = gensym "result" in
  let u (ret_ty: Ll.ty) (instr: Ll.insn): Ll.ty * Ll.operand * stream  = 
    (ret_ty, Id result, [I (result, instr)] @ stream) in
  begin match op with
    | Neg -> u I64 (Binop(Sub, I64, Const 0L, inner_op))
    | Lognot -> u I1 (Icmp(Eq, I1, inner_op, Const 0L))
    | Bitnot -> u I64 (Binop(Xor, I64, Const (-1L), inner_op))
  end

(* Compile a statement in context c with return typ rt. Return a new context, 
   possibly extended with new local bindings, and the instruction stream
   implementing the statement.

   Left-hand-sides of assignment statements must either be OAT identifiers,
   or an index into some arbitrary expression of array type. Otherwise, the
   program is not well-formed and your compiler may throw an error.

   Tips:
   - for local variable declarations, you will need to emit Allocas in the
     entry block of the current function using the E() constructor.

   - don't forget to add a bindings to the context for local variable 
     declarations

   - you can avoid some work by translating For loops to the corresponding
     While loop, building the AST and recursively calling cmp_stmt

   - you might find it helpful to reuse the code you wrote for the Call
     expression to implement the SCall statement

   - compiling the left-hand-side of an assignment is almost exactly like
     compiling the Id or Index expression. Instead of loading the resulting
     pointer, you just need to store to it!

    type elt = 
      | L of Ll.lbl             (* block labels *)
      | I of uid * Ll.insn      (* instruction *)
      | T of Ll.terminator      (* block terminators *)
      | G of gid * Ll.gdecl     (* hoisted globals (usually strings) *)
      | E of uid * Ll.insn      (* hoisted entry block instructions *)

    type stream = elt list

*)
and cmp_stmt (c:Ctxt.t) (rt:Ll.ty) (stmt:Ast.stmt node) : Ctxt.t * stream =
  begin match stmt.elt with 
    | Assn (lhs, rhs) -> 
      let _, lhs_op, lhs_stream = cmp_lhs c lhs in
      let ty, op, stream = cmp_exp c rhs in
      (c, [I ("", Store (ty, op, lhs_op))] @ stream @ lhs_stream)
    | Decl (oat_id, exp) -> 
      let ll_id = gensym oat_id in
      let ty, op, stream = cmp_exp c exp in
      (Ctxt.add c oat_id (Ptr ty, Id ll_id), [I ("", Store (ty, op, Id ll_id)); E (ll_id, Alloca ty)] @ stream)
    | Ret exp -> 
      begin match exp with 
        | None -> (c, [T (Ret (Void, None))])
        | Some exp -> let ty, op, stream = cmp_exp c exp in
          (c, [T (Ret (ty, Some op))] @ stream)
      end
    | SCall (ret, args) -> 
      let _, _, stream = cmp_exp c @@ no_loc (Call (ret, args)) in
      (c, stream)
    | If (cond_exp, ifblock, elseblock) ->
      let cond_val = gensym "cond_val" in 
      let if_c, cond_stream = cmp_stmt c rt @@ no_loc (Decl (cond_val, cond_exp)) in
      (c,
       cmp_if if_c (no_loc (Uop (Lognot, no_loc (Id cond_val)))) elseblock @
       cmp_if if_c (no_loc (Id cond_val)) ifblock @
       cond_stream)
    | For (vdecls, cond, update, body) -> 
      let while_cond = begin match cond with
        | Some x -> x
        | None -> no_loc (CBool true)
      end in 
      let while_update = begin match update with
        | Some x -> x
        | None -> no_loc (If (no_loc (CBool false), [], []))
      end in
      let block_content = (
        List.map (fun d -> Decl d) vdecls @
        [While (while_cond, body @ [while_update])]
      ) in
      (c, cmp_block c rt @@ List.map no_loc block_content)
    | While (cond, body) -> 
      let cond_lbl = gensym "cond" in 
      let body_lbl = gensym "body" in
      let end_lbl = gensym "end" in
      let cond_ty, cond_op, cond_stream = cmp_exp c cond in
      (c, [L end_lbl; T (Br cond_lbl)] @ cmp_block c Void body @ [L body_lbl] @ [T (Cbr (cond_op, body_lbl, end_lbl))] @ cond_stream @ [L cond_lbl; T (Br cond_lbl)])
  end


(* Compile a series of statements *)
and cmp_block (c:Ctxt.t) (rt:Ll.ty) (stmts:Ast.block) : stream =
  snd @@ List.fold_left (fun (c, code) s -> 
      let c, stmt_code = cmp_stmt c rt s in
      c, code >@ stmt_code
    ) (c,[]) stmts

and cmp_if c cond block : stream  = 
  let running = gensym "running" in
  cmp_block c Void [
    no_loc (Decl (running, no_loc (CBool true)));
    no_loc (While (no_loc (Bop (And, cond, no_loc (Id running))), block @ [no_loc (Assn (no_loc(Id running), no_loc (CBool false)))]));
  ]

and cmp_lhs c lhs : (Ll.ty * Ll.operand * stream) =
  begin match lhs.elt with
    | Id lhs_oat_id -> 
      let ty, op = Ctxt.lookup lhs_oat_id c in
      (ty, op, [])
    | Index (recv_exp, idx_exp) ->
      let recv_ty, recv_op, recv_stream = cmp_exp c recv_exp in
      let _, idx_op, idx_stream = cmp_exp c idx_exp in
      let el_ty = begin match recv_ty with
        | Ptr (Struct [I64; Array (_, x)]) -> x
        | Struct _ -> failwith "unsupported receiver type in index expression - Struct"
        | Array _ -> failwith "unsupported receiver type in index expression - Array"
        | Ptr _ -> failwith "unsupported receiver type in index expression - Ptr"
        | _ -> failwith "unsupported receiver type in index expression"
      end in
      let el = gensym "el" in
      (Ptr el_ty, Id el, [
          I (el, Gep (recv_ty, recv_op, [Const 0L; Const 1L; idx_op]))
        ] @ idx_stream @ recv_stream)
    | _ -> failwith "unsupported lhs"
  end

(* Generate code to allocate an array of source type TRef (RArray t) of the
   given size. Note "size" is an operand whose value can be computed at
   runtime *)
and oat_alloc_array (t:Ast.ty) (size:Ll.operand) : Ll.ty * operand * stream =
  let ans_id, arr_id = gensym "array", gensym "raw_array" in
  let temp_id = gensym "temp_array" in
  let temp_v_id = gensym "temp_array_non_ptr" in
  let i = gensym "i" in
  let n = gensym "n" in
  let ans_ty = cmp_ty @@ TRef (RArray t) in
  let temp_ty = cmp_ty @@ TRef (RArray TInt) in
  let arr_ty = Ptr I64 in
  let c_pre = Ctxt.add Ctxt.empty temp_id (Ptr temp_ty, Id temp_id) in
  let c = Ctxt.add c_pre n (Ptr I64, Id n) in
  let zero_stream = cmp_block c Void [
      no_loc @@ For (
        [(i, no_loc @@ CInt 0L)],
        Some (no_loc @@ Bop (Lt, no_loc @@ Id i, no_loc @@ Id n)),
        Some (no_loc @@ Assn (no_loc @@ Id i, no_loc @@ Bop (Add, no_loc @@ Id i, no_loc @@ CInt 1L))),
        [no_loc @@ Assn (
            no_loc @@ Index(no_loc @@ Id temp_id, no_loc @@ Id i),
            no_loc @@ CInt 0L)]
      );
    ] in
  ans_ty, Id ans_id, zero_stream @ (
      lift
        [ arr_id, Call(arr_ty, Gid "oat_alloc_array", [I64, size])
        ; ans_id, Bitcast(arr_ty, Id arr_id, ans_ty) 
        ; temp_v_id, Bitcast(arr_ty, Id arr_id, temp_ty)
        ; temp_id, Alloca temp_ty
        ; "", Store (temp_ty, Id temp_v_id, Id temp_id)
        ; n, Alloca I64
        ; "", Store (I64, size, Id n) ])


(* Adds each function identifer to the context at an
   appropriately translated type.  

   NOTE: The Gid of a function is just its source name
*)
let cmp_function_ctxt (c:Ctxt.t) (p:Ast.prog) : Ctxt.t =
  List.fold_left (fun c -> function
      | Ast.Gfdecl { elt={ frtyp; fname; args } } ->
        let ft = TRef (RFun (List.map fst args, frtyp)) in
        Ctxt.add c fname (cmp_ty ft, Gid fname)
      | _ -> c
    ) c p 

let cmp_farg (c: Ctxt.t) ((ast_ty, oat_name): (Ast.ty * Ast.id)) : (Ctxt.t * stream) =
  let local_name = gensym oat_name in
  let ll_ty = cmp_ty ast_ty in
  (
    Ctxt.add c oat_name (Ptr ll_ty, Id local_name),
    [
      I ("", Store (ll_ty, Id oat_name, Id local_name));
      E (local_name, Alloca ll_ty);
    ]
  )


(* Compile a function declaration in global context c. Return the LLVMlite cfg
   and a list of global declarations containing the string literals appearing
   in the function.

   You will need to
   1. Allocate stack space for the function parameters using Alloca
   2. Store the function arguments in their corresponding alloca'd stack slot
   3. Extend the context with bindings for function variables
   3. Compile the body of the function using cmp_block
   4. Use cfg_of_stream to produce a LLVMlite cfg from 
*)
let cmp_fdecl (c_entry:Ctxt.t) (f:Ast.fdecl node) : Ll.fdecl * (Ll.gid * Ll.gdecl) list =
  let c_body, arg_copy_stream = List.fold_left (fun (c_cur, stream) arg -> 
      let next_c, extra_stream = cmp_farg c_cur arg in
      (next_c, extra_stream @ stream)
    ) (c_entry, []) f.elt.args in
  let useless = gensym "useless" in
  let ret_ty = cmp_ret_ty f.elt.frtyp in
  let body = cfg_of_stream @@
    [T (Br useless); L useless; T (Br useless)] @
    cmp_block c_body ret_ty f.elt.body @
    arg_copy_stream in
  (
    { 
      f_ty = (List.map (fun (ty, _) -> cmp_ty ty) f.elt.args, ret_ty); 
      f_param = List.map snd f.elt.args; 
      f_cfg = fst body
    },
    snd body 
  ) 

(* Compile a global initializer, returning the resulting LLVMlite global
   declaration, and a list of additional global declarations.

   Tips:
   - Only CNull, CBool, CInt, CStr, and CArr can appear as global initializers
     in well-formed OAT programs. Your compiler may throw an error for the other
     cases

   - OAT arrays are always handled via pointers. A global array of arrays will
     be an array of pointers to arrays emitted as additional global declarations
*)
let rec cmp_gexp c (e:Ast.exp node) : Ll.gdecl * (Ll.gid * Ll.gdecl) list =
  (*
    From https://llvm.org/docs/LangRef.html#global-variables
    "Global variables always define a pointer to their ???content??? type because they describe a region of memory, and all memory objects in LLVM are accessed through pointers."

    See hw3/llprograms/global1.ll for an example
  *)
  begin match e.elt with
    | CNull ty -> (((cmp_ty ty), GNull) , [])
    | CBool v -> ((I1, GInt (if v then 1L else 0L)) , [])
    | CInt v -> ((I64, GInt v) , [])
    | CStr s -> ((Array (String.length s + 1, I8), GString s), [])
    | CArr _ -> 
      let _, decl, extra = cmp_garr c e in
      (decl, extra)
    | NewArr _ -> failwith "cmp_gexp not supported: NewArr"
    | Id _ -> failwith "cmp_gexp not supported: Id"
    | Index _ -> failwith "cmp_gexp not supported: Index"
    | Call _ -> failwith "cmp_gexp not supported: Call"
    | Bop _ -> failwith "cmp_gexp not supported: Bop"
    | Uop _ -> failwith "cmp_gexp not supported: Uop"
  end

and cmp_garr c (e: Ast.exp node) : Ll.ty * Ll.gdecl * (Ll.gid * Ll.gdecl) list = 
  begin match e.elt with
    | CArr (_, elem_exprs) -> 
      let elem_ty, elem_decls_rev, extra = List.fold_left (fun (cur_tys, cur_elem_decls, cur_extra) expr ->
          let name = gensym "elem" in
          let ty, (inner_ty, inner_init), new_extra = cmp_garr c expr in
          begin match expr.elt with
            | CArr _ -> (ty, (ty, GGid name) :: cur_elem_decls, (name, (inner_ty, inner_init)) :: new_extra @ cur_extra)
            | CStr _ -> (ty, (ty, GGid name) :: cur_elem_decls, (name, (inner_ty, inner_init)) :: new_extra @ cur_extra)
            | _ -> (ty, (inner_ty, inner_init) :: cur_elem_decls, new_extra @ cur_extra)
          end
        ) (I64, [], []) elem_exprs in (* HACK assume empty global arrays contain I64s *)
      let l = List.length elem_exprs in
      let inner_arr_ty = Array (l, elem_ty) in
      let struct_arr_ty = Struct([I64; inner_arr_ty]) in
      (Ptr struct_arr_ty,
       (struct_arr_ty, GStruct ([
            (I64, GInt (Int64.of_int l));
            (inner_arr_ty, GArray (List.rev elem_decls_rev))
          ])),
       extra)
    | CStr s ->
      let decl, extra = cmp_gexp c e in
      (Ptr (Array (String.length s + 1, I8)), decl, extra)
    | _ ->
      let ty, _, _ = cmp_exp c e in
      let decl, extra = cmp_gexp c e in
      (ty, decl, extra)
  end


(* Populate a context with bindings for global variables 
   mapping OAT identifiers to LLVMlite gids and their types.

   Only a small subset of OAT expressions can be used as global initializers
   in well-formed programs. (The constructors starting with C). 
*)
let cmp_global_ctxt (c:Ctxt.t) (p:Ast.prog) : Ctxt.t =
  let f curc decl : Ctxt.t = begin match decl with
    | Gvdecl { elt = { name; init = { elt = (CStr s) } } } -> 
      Ctxt.add curc name (Array (String.length s + 1, I8), Gid name)
    | Gvdecl { elt = { name; init = { elt = (CArr (l, elems)) } } } -> 
      let ty, _, _ = cmp_garr c @@ no_loc (CArr (l, elems)) in
      let inner_ty = begin match ty with
        | Ptr x -> x
        | _ -> failwith "expected pointer"
      end in
      Ctxt.add curc name (inner_ty, Gid name)
    | Gvdecl { elt = { name; init } } -> 
      let ty, _, _ = cmp_exp c init in
      Ctxt.add curc name (Ptr ty, Gid name)
    | Gfdecl node -> curc
  end in
  List.fold_left f c p 


(* Oat internals function context ------------------------------------------- *)
let internals = [
  "oat_alloc_array",         Ll.Fun ([I64], Ptr I64)
]

(* Oat builtin function context --------------------------------------------- *)
let builtins =
  [ "array_of_string",  cmp_rty @@ RFun ([TRef RString], RetVal (TRef(RArray TInt)))
  ; "string_of_array",  cmp_rty @@ RFun ([TRef(RArray TInt)], RetVal (TRef RString))
  ; "length_of_string", cmp_rty @@ RFun ([TRef RString],  RetVal TInt)
  ; "string_of_int",    cmp_rty @@ RFun ([TInt],  RetVal (TRef RString))
  ; "string_cat",       cmp_rty @@ RFun ([TRef RString; TRef RString], RetVal (TRef RString))
  ; "print_string",     cmp_rty @@ RFun ([TRef RString],  RetVoid)
  ; "print_int",        cmp_rty @@ RFun ([TInt],  RetVoid)
  ; "print_bool",       cmp_rty @@ RFun ([TBool], RetVoid)
  ]

(* Compile a OAT program to LLVMlite *)
let cmp_prog (p:Ast.prog) : Ll.prog =
  (* add built-in functions to context *)
  let init_ctxt = 
    List.fold_left (fun c (i, t) -> Ctxt.add c i (Ll.Ptr t, Gid i))
      Ctxt.empty builtins
  in
  let fc = cmp_function_ctxt init_ctxt p in

  (* build global variable context *)
  let c = cmp_global_ctxt fc p in

  (* compile functions and global variables *)
  let fdecls, gdecls = 
    List.fold_right (fun d (fs, gs) ->
        match d with
        | Ast.Gvdecl { elt=gd } -> 
          let ll_gd, gs' = cmp_gexp c gd.init in
          (fs, (gd.name, ll_gd)::gs' @ gs)
        | Ast.Gfdecl fd ->
          let fdecl, gs' = cmp_fdecl c fd in
          (fd.elt.fname,fdecl)::fs, gs' @ gs
      ) p ([], [])
  in

  (* gather external declarations *)
  let edecls = internals @ builtins in
  { tdecls = []; gdecls; fdecls; edecls }