(** Data structures, signatures  *)

(** Comparable, printable type *)
module type OrdPrintT =
sig
  type t
  val compare : t -> t -> int
  val to_string : t -> string
end

(** Extended sets *)
module type SetS =
sig
  include Set.S
  val of_list : elt list -> t     (* added to Set.S in OCaml 4.02 *)
  val to_string : t -> string
  val string_of_elt : elt -> string
  val printer : Format.formatter -> t -> unit
end

module MakeSet (Ord : OrdPrintT) : SetS with type elt = Ord.t =
struct
  include Set.Make (Ord)

  let of_list = List.fold_left (fun s e -> add e s) empty

  let to_string t =
    let s = elements t
            |> List.map Ord.to_string
            |> String.concat ", "
    in
    "{" ^ s ^ "}"

  let string_of_elt = Ord.to_string

  let printer f t = Format.pp_print_string f (to_string t)

end

(** Extended maps *)
module type MapS =
sig
  include Map.S
  val update : ('a -> 'a) -> key -> 'a t -> 'a t
  val find_or : 'a -> 'a t -> key -> 'a
  val update_or : 'a -> ('a -> 'a) -> key -> 'a t -> 'a t
  val diff_keys : ('a -> 'a -> int) -> 'a t -> 'a t -> key list
  val to_string : (key -> 'a -> string) -> 'a t -> string
  val printer : (key -> 'a -> string) -> Format.formatter -> 'a t -> unit
end

module MakeMap (Ord : OrdPrintT) : MapS with type key = Ord.t =
struct
  include Map.Make (Ord)

  let update f k m =
    add k (f @@ find k m) m

  let find_or d m k =
    try find k m with Not_found -> d

  let diff_keys cmp_v m n =
    let module S = MakeSet(Ord) in
    let has_binding_or_add m k v l =
      try if cmp_v v @@ find k m == 0 then l else S.add k l
      with Not_found -> S.add k l
    in
    S.empty |> fold (has_binding_or_add n) m
    |> fold (has_binding_or_add m) n
    |> S.elements

  let update_or d f k m = 
    add k (f @@ find_or d m k) m

  let to_string val_str t =
    let s = bindings t
            |> List.map (fun (k,v) -> Ord.to_string k ^ "=" ^ val_str k v)
            |> String.concat ", "
    in
    "{" ^ s ^ "}"

  let printer val_str f t = Format.pp_print_string f (to_string val_str t)

end

(** Useful instances *)

module Lbl =
struct
  type t = Ll.lbl
  let compare = String.compare
  let to_string l = l
end

module LblM = MakeMap (Lbl)
module LblS = MakeSet (Lbl)

module Uid =
struct
  type t = Ll.uid
  let compare = String.compare
  let to_string u = "%" ^ u
end

module UidS = MakeSet (Uid)
module UidM = MakeMap (Uid)

module UidPair =
struct
  type t = Uid.t * Uid.t
  let compare = Pervasives.compare
  let to_string (a, b) = Printf.sprintf "(%%%s, %%%s)" a b
end

module UidPairS = MakeSet (UidPair)

module UidGraph = struct
  type t = UidS.t * UidPairS.t
  let empty: t = (UidS.empty, UidPairS.empty)

  let canonical_pair ((a, b): UidPair.t) : UidPair.t =
    if String.compare a b < 0 then (a, b) else (b, a)
  (* module FindIndex = Map.Make(Uid * Int) *)

  let add_node (uid: Uid.t) ((us, ups): t) : t = (UidS.add uid us, ups)
  let remove_node (uid: Uid.t) ((us, ups): t) : t = (UidS.remove uid us, ups)
  let get_nodes (us, ups): UidS.t = us
  let add_edge (p: UidPair.t) ((us, ups): t) : t = 
    if not @@ UidS.mem (fst p) us || not @@ UidS.mem (snd p) us
    then failwith @@ Printf.sprintf "tried to add edge between nodes which do not exist: (%%%s, %%%s)" (fst p) (snd p)
    else (us, UidPairS.add (canonical_pair p) ups)
  let fully_connect (uid_set: UidS.t) (g: t) : t = 
    let uids = UidS.to_seq uid_set in
    let pairs =
      Seq.flat_map (fun a -> Seq.map (fun b -> (a, b)) uids) uids 
      |> Seq.filter (fun (a, b) -> a != b) in
    Seq.fold_left (fun g p -> add_edge p g) g pairs
  let find_node_with_deg_less_than (k: int) ((us, ups): t) : Uid.t option =
    let instances = ups
                    |> UidPairS.to_seq
                    |> Seq.flat_map (fun (a, b) -> List.to_seq @@ List.sort_uniq String.compare [a; b]) 
                    |> List.of_seq 
                    |> List.sort String.compare in
    let res_with_count = List.fold_left (fun res uid -> 
        begin match res with
          | None -> Some (uid, 1)
          | Some (other_uid, n) ->
            if uid = other_uid then Some (uid, n + 1)
            else if n < k then res
            else Some (uid, 1)
        end
      ) None instances in
    let pre_res = begin match res_with_count with
      | None -> None
      | Some (uid, n) -> if n < k then Some uid else None
    end in
    begin match (pre_res, k > 0) with
      | (None, true) -> UidS.choose_opt @@ UidS.diff us @@ UidS.of_list instances
      | _ -> pre_res
    end
  let get_neighbours (uid: Uid.t) ((us, ups) : t) : UidS.t = 
    UidS.filter (fun a -> UidPairS.mem (canonical_pair(a, uid)) ups) us
  let find_node_with_deg_less_than_naive (k: int) ((us, ups): t) : Uid.t option =
    UidS.choose_opt @@ UidS.filter (fun a -> k > List.length @@ UidS.elements @@ get_neighbours a (us, ups)) us
end

(** For testing   *)
let uidm (b:(Ll.uid * 'a) list) : 'a UidM.t =
  List.fold_left (fun m (k,v) -> UidM.add k v m) UidM.empty b

let lblm (b:(Ll.lbl * 'a) list) : 'a LblM.t =
  List.fold_left (fun m (k,v) -> LblM.add k v m) LblM.empty b

let uids (l:Ll.uid list) : UidS.t = UidS.of_list l
let lbls (l:Ll.lbl list) : LblS.t = LblS.of_list l
