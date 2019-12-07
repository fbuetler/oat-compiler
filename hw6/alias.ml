(** Alias Analysis *)

open Ll
open Datastructures

(* The lattice of abstract pointers ----------------------------------------- *)
module SymPtr =
struct
  type t = MayAlias           (* uid names a pointer that may be aliased *)
         | Unique             (* uid is the unique name for a pointer *)
         | UndefAlias         (* uid is not in scope or not a pointer *)

  let compare : t -> t -> int = Pervasives.compare

  let to_string = function
    | MayAlias -> "MayAlias"
    | Unique -> "Unique"
    | UndefAlias -> "UndefAlias"

end

(* The analysis computes, at each program point, which UIDs in scope are a unique name
   for a stack slot and which may have aliases *)
type fact = SymPtr.t UidM.t

(* flow function across Ll instructions ------------------------------------- *)
(* TASK: complete the flow function for alias analysis. 

   - After an alloca, the defined UID is the unique name for a stack slot
   - A pointer returned by a load, call, bitcast, or GEP may be aliased
   - A pointer passed as an argument to a call, bitcast, GEP, or store
     may be aliased
   - Other instructions do not define pointers

*)
let insn_flow ((u,i):uid * insn) (d:fact) : fact =
  let (uid_is_unique: bool), (aliased_ops: Ll.operand list) = begin match i with
    | Binop (op, ty, a, b) -> false, []
    | Alloca ty -> true, []
    | Load (ty, op) -> begin match ty with 
        | Ptr Ptr _ -> false, [Id u]
        | _ -> false, []
      end
    | Store (ty, src, dest) -> begin match ty with 
        | Ptr _ -> false, [src]
        | _ -> false, []
      end
    | Icmp (ty, op, a, b) -> false, []
    | Call (ret, f, args) -> false, Id u :: List.map snd args
    | Bitcast (in_ty, op, out_ty) -> false, [Id u; op] 
    | Gep (ty, op, l) -> false, [Id u; op]
  end in
  let philippes_stuff = List.map (fun op -> begin match op with
      | Id uid -> Some (uid, SymPtr.MayAlias)
      | _ -> None 
    end) aliased_ops in
  let philippes_stuff = (if uid_is_unique then Some (u, SymPtr.Unique) else None) :: philippes_stuff in
  List.fold_left (fun acc el -> begin match el with
      | Some (uid, sym) -> UidM.add uid sym acc
      | None -> acc
    end) d philippes_stuff

(* The flow function across terminators is trivial: they never change alias info *)
let terminator_flow t (d:fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact =
struct
  type t = fact
  let forwards = true

  let insn_flow = insn_flow
  let terminator_flow = terminator_flow

  (* UndefAlias is logically the same as not having a mapping in the fact. To
     compare dataflow facts, we first remove all of these *)
  let normalize : fact -> fact = 
    UidM.filter (fun _ v -> v != SymPtr.UndefAlias)

  let compare (d:fact) (e:fact) : int = 
    UidM.compare SymPtr.compare (normalize d) (normalize e)

  let to_string : fact -> string =
    UidM.to_string (fun _ v -> SymPtr.to_string v)

  (* TASK: complete the "combine" operation for alias analysis.

     The alias analysis should take the join over predecessors to compute the
     flow into a node. You may find the UidM.merge function useful.

     It may be useful to define a helper function that knows how to take the
     join of two SymPtr.t facts.

     may + may -> may
     may + unique -> may
     may + undef -> undef
     unique + undef -> undef
     unique + unique -> unique
     undef + undef -> undef
  *)
  let combine (ds:fact list) : fact =
    List.fold_left (UidM.union (fun _ a b ->
        let res = List.find
            (fun c -> a = c || b = c)
            [SymPtr.UndefAlias; SymPtr.MayAlias; SymPtr.Unique] in
        Some res
      )) UidM.empty ds
end

(* instantiate the general framework ---------------------------------------- *)
module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* expose a top-level analysis operation ------------------------------------ *)
let analyze (g:Cfg.t) : Graph.t =
  (* the analysis starts with every node set to bottom (the map of every uid 
     in the function to UndefAlias *)
  let init l = UidM.empty in

  (* the flow into the entry node should indicate that any pointer parameter 
     to the function may be aliased *)
  let alias_in = 
    List.fold_right 
      (fun (u,t) -> match t with
         | Ptr _ -> UidM.add u SymPtr.MayAlias
         | _ -> fun m -> m) 
      g.Cfg.args UidM.empty 
  in
  let fg = Graph.of_cfg init alias_in g in
  Solver.solve fg

