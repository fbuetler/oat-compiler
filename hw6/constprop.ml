open Ll
open Datastructures

(* The lattice of symbolic constants ---------------------------------------- *)
module SymConst =
struct
  type t = NonConst           (* Uid may take on multiple values at runtime *)
         | Const of int64     (* Uid will always evaluate to const i64 or i1 *)
         | UndefConst         (* Uid is not defined at the point *)

  let compare s t =
    match (s, t) with
    | (Const i, Const j) -> Int64.compare i j
    | (NonConst, NonConst) | (UndefConst, UndefConst) -> 0
    | (NonConst, _) | (_, UndefConst) -> 1
    | (UndefConst, _) | (_, NonConst) -> -1

  let to_string : t -> string = function
    | NonConst -> "NonConst"
    | Const i -> Printf.sprintf "Const (%LdL)" i
    | UndefConst -> "UndefConst"


end

(* The analysis computes, at each program point, which UIDs in scope will evaluate 
   to integer constants *)
type fact = SymConst.t UidM.t



(* flow function across Ll instructions ------------------------------------- *)
(* - Uid of a binop or icmp with const arguments is constant-out
   - Uid of a binop or icmp with an UndefConst argument is UndefConst-out
   - Uid of a binop or icmp with an NonConst argument is NonConst-out
   - Uid of stores and void calls are UndefConst-out
   - Uid of all other instructions are NonConst-out
*)
let insn_flow (u,i:uid * insn) (d:fact) : fact =
  let get_op (op: Ll.operand) : SymConst.t = begin match op with
    | Id uid -> UidM.find_or SymConst.UndefConst d uid
    | Const v -> SymConst.Const v
    | _ -> SymConst.NonConst
  end in
  let stuff (a: Ll.operand) (b: Ll.operand) (f: int64 -> int64 -> int64) : SymConst.t = 
    begin match (get_op a, get_op b) with
      | (SymConst.Const va, SymConst.Const vb) -> SymConst.Const (f va vb)
      | (SymConst.NonConst, _) | (_, SymConst.NonConst) -> SymConst.NonConst
      | (SymConst.UndefConst, _) | (_, SymConst.UndefConst) -> SymConst.UndefConst
    end
  in
  let out_sym: SymConst.t = begin match i with
    | Binop (bop, _, a, b) -> stuff a b (begin match bop with
        | Add -> Int64.add
        | Sub -> Int64.sub
        | Mul -> Int64.mul
        | Shl -> (fun a b -> Int64.shift_left a (Int64.to_int b))
        | Lshr -> (fun a b -> Int64.shift_right_logical a (Int64.to_int b))
        | Ashr -> (fun a b -> Int64.shift_right a (Int64.to_int b))
        | And -> Int64.logand
        | Or -> Int64.logor
        | Xor -> Int64.logxor 
      end)
    | Icmp (cnd, _, a, b) -> stuff a b (fun a b -> 
        let bool_val = begin match cnd with
          | Eq -> Int64.equal a b
          | Ne -> not @@ Int64.equal a b
          | Slt -> Int64.compare a b < 0
          | Sle -> Int64.compare a b <= 0
          | Sgt -> Int64.compare a b > 0
          | Sge -> Int64.compare a b >= 0
        end in
        if bool_val then 1L else 0L
      )
    | Store _ -> SymConst.UndefConst
    | Call (Void, _, _) -> SymConst.UndefConst
    | _ -> SymConst.NonConst
  end in
  UidM.add u out_sym d

(* The flow function across terminators is trivial: they never change const info *)
let terminator_flow (t:terminator) (d:fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact =
struct
  type t = fact
  let forwards = true

  let insn_flow = insn_flow
  let terminator_flow = terminator_flow

  let normalize : fact -> fact = 
    UidM.filter (fun _ v -> v != SymConst.UndefConst)

  let compare (d:fact) (e:fact) : int  = 
    UidM.compare SymConst.compare (normalize d) (normalize e)

  let to_string : fact -> string =
    UidM.to_string (fun _ v -> SymConst.to_string v)

  (* The constprop analysis should take the join over predecessors to compute the
     flow into a node. You may find the UidM.merge function useful 
  *)
  let combine (ds:fact list) : fact = 
    (* Printf.printf "combine %d\n" @@ List.length ds; *)
    List.fold_left (UidM.union (fun _ a b ->
        (* Printf.printf "combine (%s, %s)\n" (SymConst.to_string a) (SymConst.to_string b); *)
        begin match (a, b, a == b) with
          | (SymConst.Const _, SymConst.Const _, true) -> Some a
          | (SymConst.Const _, SymConst.Const _, false) -> Some SymConst.NonConst
          (* TODO should NonConst or UndefConst come first? *)
          | (SymConst.NonConst, _, _) | (_, SymConst.NonConst, _) -> Some SymConst.NonConst
          | (SymConst.UndefConst, _, _) | (_, SymConst.UndefConst, _) -> Some SymConst.UndefConst
        end)) UidM.empty ds
end

(* instantiate the general framework ---------------------------------------- *)
module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* expose a top-level analysis operation ------------------------------------ *)
let analyze (g:Cfg.t) : Graph.t =
  (* the analysis starts with every node set to bottom (the map of every uid 
     in the function to UndefConst *)
  let init l = UidM.empty in

  (* the flow into the entry node should indicate that any parameter to the
     function is not a constant *)
  let cp_in = List.fold_right 
      (fun (u,_) -> UidM.add u SymConst.NonConst)
      g.Cfg.args UidM.empty 
  in
  let fg = Graph.of_cfg init cp_in g in
  Solver.solve fg


(* run constant propagation on a cfg given analysis results ----------------- *)
(* HINT: your cp_block implementation will probably rely on several helper 
   functions.                                                                 *)
let run (cg:Graph.t) (cfg:Cfg.t) : Cfg.t =
  let open SymConst in

  let map_operands (i: Ll.insn) (f: Ll.operand -> Ll.operand) : Ll.insn = begin match i with
    | Binop (op, ty, a, b) -> Binop (op, ty, f a, f b)
    | Alloca ty -> Alloca ty
    | Load (ty, op) -> Load (ty, f op)
    | Store (ty, src, dest) -> Store (ty, f src, f dest)
    | Icmp (ty, op, a, b) -> Icmp (ty, op, f a, f b)
    | Call (ret, func, args) -> Call (ret, func, List.map (fun (ty, op) -> (ty, f op)) args)
    | Bitcast (in_ty, op, out_ty) -> Bitcast (in_ty, f op, out_ty)
    | Gep (ty, op, l) -> Gep (ty, f op, l)
  end in

  let map_operands_term (term: Ll.terminator) (f: Ll.operand -> Ll.operand) : Ll.terminator = begin match term with
    | Ret (ty, op_opt) -> Ret (ty, begin match op_opt with
        | Some op -> Some (f op)
        | None -> None
      end)
    | Br l -> Br l
    | Cbr (op, l_if, l_else) -> Cbr (f op, l_if, l_else)
  end in

  let cp_block (l:Ll.lbl) (cfg:Cfg.t) : Cfg.t =
    let b = Cfg.block cfg l in
    let cb = Graph.uid_out cg l in
    let get_new_op uid op = begin match op with
      | Id op_uid -> begin match UidM.find_opt op_uid @@ cb uid with
          | Some Const v -> Ll.Const v
          | _ -> op
        end
      | _ -> op
    end in
    let new_insns = List.map (fun (uid, instr) -> 
        (uid, map_operands instr @@ get_new_op uid)
      ) b.insns in
    Cfg.add_block l {
      insns = new_insns;
      term = (fst b.term, map_operands_term (snd b.term) @@ get_new_op (fst b.term));
    } cfg
  in
  LblS.fold cp_block (Cfg.nodes cfg) cfg
