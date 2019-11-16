open Ast
open Astlib
open Tctxt

(* Error Reporting ---------------------------------------------------------- *)
(* NOTE: Use type_error to report error messages for ill-typed programs. *)

exception TypeError of string

let type_error (l : 'a node) err = 
  let (_, (s, e), _) = l.loc in
  raise (TypeError (Printf.sprintf "[%d, %d] %s" s e err))

module SAstField = Set.Make(
  struct 
    let compare = Pervasives.compare
    type t = Ast.field
  end
  );;

module SS = Set.Make(String);;

let sort_by_str (get_key: 'a -> string) (l: 'a list) : 'a list =
  List.sort (fun a b -> String.compare (get_key a) (get_key b)) l

let find_duplicate_string (l : string list) : string option =
  fst @@ List.fold_left (fun (dup, seen) s ->
      begin match (dup, SS.find_opt s seen) with
        | Some _, _ -> (dup, seen)
        | None, Some _ -> (Some s, seen)
        | None, None -> (None, SS.add s seen)
      end
    ) (None, SS.empty) l

(* initial context: G0 ------------------------------------------------------ *)
(* The Oat types of the Oat built-in functions *)
let builtins =
  [ "array_of_string",  ([TRef RString],  RetVal (TRef(RArray TInt)))
  ; "string_of_array",  ([TRef(RArray TInt)], RetVal (TRef RString))
  ; "length_of_string", ([TRef RString],  RetVal TInt)
  ; "string_of_int",    ([TInt], RetVal (TRef RString))
  ; "string_cat",       ([TRef RString; TRef RString], RetVal (TRef RString))
  ; "print_string",     ([TRef RString],  RetVoid)
  ; "print_int",        ([TInt], RetVoid)
  ; "print_bool",       ([TBool], RetVoid)
  ]

(* binary operation types --------------------------------------------------- *)
let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)
  | Eq | Neq -> failwith "typ_of_binop called on polymorphic == or !="

(* unary operation types ---------------------------------------------------- *)
let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)

(* subtyping ---------------------------------------------------------------- *)
(* Decides whether H |- t1 <: t2 
   - assumes that H contains the declarations of all the possible struct types

   - you will want to introduce addition (possibly mutually recursive) 
      helper functions to implement the different judgments of the subtyping
      relation. We have included a template for subtype_ref to get you started.
      (Don't forget about OCaml's 'and' keyword.)
*)
let rec subtype (c : Tctxt.t) (t1 : Ast.ty) (t2 : Ast.ty) : bool =
  begin match (t1, t2, t1 == t2) with
    | (_, _, true) -> true
    | (TNullRef r1, TNullRef r2, _) -> subtype_ref c r1 r2
    | (TRef r1, TRef r2, _) -> subtype_ref c r1 r2
    | (TRef r1, TNullRef r2, _) -> subtype_ref c r1 r2
    | _ -> false
  end 

(* Decides whether H |-r ref1 <: ref2 *)
and subtype_ref (c : Tctxt.t) (t1 : Ast.rty) (t2 : Ast.rty) : bool =
  begin match (t1, t2, t1 == t2) with
    | (_, _, true) -> true
    | (RStruct s1, RStruct s2, _) -> subtype_struct (lookup_struct s1 c) (lookup_struct s2 c)
    | (RFun (t1, rt1), RFun (t2, rt2), _) ->
      (subtype_return c rt1 rt2) &&
      (List.length t1 == List.length t2) &&
      (List.fold_left (&&) true @@ List.map2 (subtype c) t2 t1)
    | _ -> false
  end

and subtype_struct (s1: Ast.field list) (s2: Ast.field list) : bool =
  SAstField.subset (SAstField.of_list s2) (SAstField.of_list s1)

and subtype_return (c : Tctxt.t) (t1 : Ast.ret_ty) (t2 : Ast.ret_ty) : bool =
  begin match (t1, t2, t1 == t2) with
    | (_, _, true) -> true
    | (RetVal v1, RetVal v2, _) -> subtype c v1 v2
    | _ -> false
  end


(* well-formed types -------------------------------------------------------- *)
(* Implement a (set of) functions that check that types are well formed according
   to the H |- t and related inference rules

   - the function should succeed by returning () if the type is well-formed
      according to the rules

   - the function should fail using the "type_error" helper function if the 
      type is 

   - l is just an ast node that provides source location information for
      generating error messages (it's only needed for the type_error generation)

   - tc contains the structure definition context
*)
let rec typecheck_ty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ty) : unit =
  begin match t with
    | TBool -> ()
    | TInt -> ()
    | TRef x -> typecheck_rty l tc x
    | TNullRef x -> typecheck_rty l tc x
  end

and typecheck_rty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.rty) : unit =
  begin match t with
    | RString -> ()
    | RStruct name -> begin match lookup_struct_option name tc with
        | Some _ -> ()
        | None -> type_error l @@ Printf.sprintf "Struct id %s not found in ctxt" name
      end
    | RArray el -> typecheck_ty l tc el
    | RFun (args, r) ->
      typecheck_ret_ty l tc r;
      List.iter (typecheck_ty l tc) args;
  end

and typecheck_ret_ty  (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ret_ty) : unit =
  begin match t with
    | RetVoid -> ()
    | RetVal x -> typecheck_ty l tc x
  end

let add_new_local (c : Tctxt.t) (node: 'a node) (id : id) (bnd : Ast.ty) : Tctxt.t =
  begin match lookup_option id c with
    | Some _ -> type_error node @@ Printf.sprintf "can not redeclare variable %s" id
    | None -> add_local c id bnd
  end

(* typechecking expressions ------------------------------------------------- *)
(* Typechecks an expression in the typing context c, returns the type of the
   expression.  This function should implement the inference rules given in the
   oad.pdf specification.  There, they are written:

       H; G; L |- exp : t

   See tctxt.ml for the implementation of the context c, which represents the
   four typing contexts: H - for structure definitions G - for global
   identifiers L - for local identifiers

   Returns the (most precise) type for the expression, if it is type correct
   according to the inference rules.

   Uses the type_error function to indicate a (useful!) error message if the
   expression is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   Notes: - Structure values permit the programmer to write the fields in any
   order (compared with the structure definition).  This means that, given the
   declaration struct T { a:int; b:int; c:int } The expression new T {b=3; c=4;
   a=1} is well typed.  (You should sort the fields to compare them.)

*)
let rec typecheck_exp (c : Tctxt.t) (e : Ast.exp node) : Ast.ty =
  let get_struct (struct_name : string) = 
    begin match lookup_struct_option struct_name c with 
      | Some fields -> fields
      | None -> type_error e @@ Printf.sprintf "struct %s does not exist in ctxt" struct_name
    end in
  begin match e.elt with
    | CNull rty -> TNullRef rty
    | CBool b -> TBool
    | CInt i -> TInt
    | CStr s -> TRef RString
    | Id id -> lookup id c 
    | CArr (ty, l) -> List.iter (assert_exp_type c ty) l; TRef (RArray ty)
    | NewArr (ty, len_exp, index, init_exp) ->
      assert_exp_type c TInt len_exp;
      assert_exp_type (add_new_local c e index TInt) ty init_exp;
      TRef (RArray ty)
    | Index (arr, index) -> 
      assert_exp_type c TInt index; 
      assert_exp_type_any_array c arr
    | Length arr -> ignore @@ assert_exp_type_any_array c arr; TInt
    | CStruct (struct_name, field_vals) ->
      begin match find_duplicate_string @@ List.map fst field_vals with
        | Some s -> type_error e @@ Printf.sprintf "duplicate field %s in struct literal" s
        | None -> ()
      end; 
      let field_defs = get_struct struct_name in 
      if List.length field_defs <> List.length field_vals
      then type_error e @@ Printf.sprintf "struct %s: expected %d fields, but got %d" struct_name (List.length field_defs) (List.length field_vals);
      let groups = List.map2
          (fun { ftyp } (n, v) -> (n, ftyp, v))
          (sort_by_str (fun e -> e.fieldName) field_defs)
          (sort_by_str fst field_vals) in
      List.iter (fun (n, ftyp, v) -> 
          let actual_ty = typecheck_exp c v in
          if not @@ subtype c actual_ty ftyp
          then type_error e @@ Printf.sprintf "field %s of %s: expected %s, but got %s" n struct_name (string_of_ty ftyp) (string_of_ty actual_ty)
        ) groups;
      TRef (RStruct struct_name)
    | Proj (exp, field_name) -> 
      let struct_name = begin match typecheck_exp c exp with
        | TRef (RStruct x) -> x
        | ty -> type_error exp @@ Printf.sprintf "expected struct, but got %s" @@ string_of_ty ty
      end in 
      let fields = get_struct struct_name in 
      begin match List.find_opt (fun el -> el.fieldName == field_name) fields with
        | Some { ftyp } -> ftyp
        | None -> type_error e @@ Printf.sprintf "struct %s does not have field %s" struct_name field_name 
      end
    | Call (f_exp, args) -> 
      let arg_tys, r_ty = begin match typecheck_exp c f_exp with 
        | TRef (RFun (arg_tys, r_ty)) -> (arg_tys, r_ty)
        | _ -> type_error e "expression is not a function"
      end in
      if List.length arg_tys <> List.length args
      then type_error e @@ Printf.sprintf "expected %d arguments, but got %d" (List.length arg_tys) (List.length args);
      let groups = List.map2 (fun ty v -> (ty, v)) arg_tys args in
      List.iteri (fun i (ty, v) -> 
          let actual_ty = typecheck_exp c v in
          if not @@ subtype c actual_ty ty
          then type_error e @@ Printf.sprintf "argument %d: expected %s, but got %s" (i + 1) (string_of_ty ty) (string_of_ty actual_ty)
        ) groups;
      begin match r_ty with
        | RetVoid -> type_error e "call of void function is not an expression"
        | RetVal ty -> ty
      end
    | Bop (op, l, r) -> begin match op with
        | Add | Sub | Mul | IAnd | IOr | Shl | Shr | Sar ->
          assert_exp_type c TInt l; assert_exp_type c TInt r; TInt
        | Eq | Neq | Lt | Lte | Gt | Gte ->
          assert_exp_type c TInt l; assert_exp_type c TInt r; TBool
        | And | Or ->
          assert_exp_type c TBool l; assert_exp_type c TBool r; TBool
      end
    | Uop (op, r) -> begin match op with
        | Neg -> assert_exp_type c TInt r; TInt
        | Lognot -> assert_exp_type c TBool r; TBool
        | Bitnot -> assert_exp_type c TInt r; TInt
      end
  end

and assert_exp_type (tc: Tctxt.t) (expected_ty : Ast.ty) (exp : Ast.exp node) : unit =
  let actual_ty = typecheck_exp tc exp in
  if not @@ subtype tc actual_ty expected_ty
  then type_error exp @@ Printf.sprintf "expected %s, but got %s" (string_of_ty expected_ty) (string_of_ty actual_ty)

and assert_exp_type_any_array (tc: Tctxt.t) (exp: Ast.exp node) : ty =
  begin match typecheck_exp tc exp with
    | TRef (RArray ty) -> ty
    | ty -> type_error exp @@ Printf.sprintf "expected array, but got %s" @@ string_of_ty ty
  end

(* statements --------------------------------------------------------------- *)

(* Typecheck a statement 
   This function should implement the statment typechecking rules from oat.pdf.  

   Inputs:
   - tc: the type context
   - s: the statement node
   - to_ret: the desired return type (from the function declaration)

   Returns:
   - the new type context (which includes newly declared variables in scope
       after this statement
   - A boolean indicating the return behavior of a statement:
        false:  might not return
        true: definitely returns 

        in the branching statements, both branches must definitely return

        Intuitively: if one of the two branches of a conditional does not 
        contain a return statement, then the entier conditional statement might 
        not return.

        looping constructs never definitely return 

   Uses the type_error function to indicate a (useful!) error message if the
   statement is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   - You will probably find it convenient to add a helper function that implements the 
     block typecheck rules.
*)
let rec typecheck_stmt (tc : Tctxt.t) (s:Ast.stmt node) (to_ret:ret_ty) : Tctxt.t * bool =
  failwith "todo: implement typecheck_stmt"


(* struct type declarations ------------------------------------------------- *)
(* Here is an example of how to implement the TYP_TDECLOK rule, which is 
   is needed elswhere in the type system.
*)

(* Helper function to look for duplicate field names *)
let rec check_dups fs =
  match fs with
  | [] -> false
  | h :: t -> (List.exists (fun x -> x.fieldName = h.fieldName) t) || check_dups t

let typecheck_tdecl (tc : Tctxt.t) id fs  (l : 'a Ast.node) : unit =
  if check_dups fs
  then type_error l ("Repeated fields in " ^ id) 
  else List.iter (fun f -> typecheck_ty l tc f.ftyp) fs

(* function declarations ---------------------------------------------------- *)
(* typecheck a function declaration 
   - extends the local context with the types of the formal parameters to the 
      function
   - typechecks the body of the function (passing in the expected return type
   - checks that the function actually returns
*)
let typecheck_fdecl (tc : Tctxt.t) (f : Ast.fdecl) (l : 'a Ast.node) : unit =
  failwith "todo: typecheck_fdecl"

(* creating the typchecking context ----------------------------------------- *)

(* The following functions correspond to the
   judgments that create the global typechecking context.

   create_struct_ctxt: - adds all the struct types to the struct 'S'
   context (checking to see that there are no duplicate fields

     H |-s prog ==> H'


   create_function_ctxt: - adds the the function identifiers and their
   types to the 'F' context (ensuring that there are no redeclared
   function identifiers)

     H ; G1 |-f prog ==> G2


   create_global_ctxt: - typechecks the global initializers and adds
   their identifiers to the 'G' global context

     H ; G1 |-g prog ==> G2    


   NOTE: global initializers may mention function identifiers as
   constants, but can't mention other global values *)

let create_struct_ctxt (p:Ast.prog) : Tctxt.t =
  failwith "todo: create_struct_ctxt"

let create_function_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  failwith "todo: create_function_ctxt"

let create_global_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  failwith "todo: create_function_ctxt"


(* This function implements the |- prog and the H ; G |- prog 
   rules of the oat.pdf specification.   
*)
let typecheck_program (p:Ast.prog) : unit =
  let sc = create_struct_ctxt p in
  let fc = create_function_ctxt sc p in
  let tc = create_global_ctxt fc p in
  List.iter (fun p ->
      match p with
      | Gfdecl ({elt=f} as l) -> typecheck_fdecl tc f l
      | Gtdecl ({elt=(id, fs)} as l) -> typecheck_tdecl tc id fs l 
      | _ -> ()) p
