open Assert
open Datastructures

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let assert_set_eq (actual: UidS.t) (expected: Uid.t list) : Assert.assertion =
  assert_eq (UidS.equal actual @@ UidS.of_list expected) true

let assert_graph_eq ((actual_nodes, actual_edges): UidGraph.t) (expected_nodes: Uid.t list) (expected_edges: UidPair.t list) : Assert.assertion =
  assert_eq (
    (UidS.equal actual_nodes @@ UidS.of_list expected_nodes) &&
    (UidPairS.equal actual_edges @@ UidPairS.of_list @@ List.map UidGraph.canonical_pair expected_edges)
  ) true

let assert_find_with_deg (g: UidGraph.t) (k: int) (expected: Uid.t list) : Assert.assertion =
  begin match UidGraph.find_node_with_deg_less_than k g with
    | None -> assert_eq (List.length expected) 0
    | Some uid -> assert_eq (List.mem uid expected) true
  end

let assert_find_with_deg_naive (g: UidGraph.t) (k: int) (expected: Uid.t list) : Assert.assertion =
  begin match UidGraph.find_node_with_deg_less_than_naive k g with
    | None -> assert_eq (List.length expected) 0
    | Some uid -> assert_eq (List.mem uid expected) true
  end

let test_graph_nodes = ["a"; "b"; "c"; "d"]
let test_graph_edges = [("a", "b"); ("b", "c")]
let test_graph : UidGraph.t = (UidS.of_list test_graph_nodes, UidPairS.empty)
let test_graph_with_edges : UidGraph.t = (UidS.of_list test_graph_nodes, UidPairS.of_list test_graph_edges)

let provided_tests : suite = [
  Test("UidGraph.get_nodes", [
      "there is no node", assert_set_eq (UidGraph.get_nodes @@ UidGraph.empty) [];
      "there is only 1 node", assert_set_eq (UidGraph.get_nodes @@ (UidS.of_list ["a"], UidPairS.empty)) ["a"];
      "there assert_set_eq 2 nodes", assert_set_eq (UidGraph.get_nodes @@ (UidS.of_list ["a"; "b"], UidPairS.empty)) ["a"; "b"];
      "there assert_set_eq 3 nodes", assert_set_eq (UidGraph.get_nodes @@ (UidS.of_list ["a"; "b"; "c"], UidPairS.empty)) ["a"; "b"; "c"];
    ]);
  Test("UidGraph.empty", [
      "has no nodes", assert_set_eq (UidGraph.get_nodes UidGraph.empty) [];
    ]);
  Test("UidGraph.add_node", [
      "adds a first node", assert_set_eq (UidGraph.get_nodes @@ UidGraph.add_node "a" UidGraph.empty) ["a"];
      "re-add existing node", assert_set_eq (UidGraph.get_nodes @@ UidGraph.add_node "a" @@ UidGraph.add_node "a" UidGraph.empty) ["a"];
      "add multiple", assert_set_eq (UidGraph.get_nodes @@ UidGraph.add_node "a" @@ UidGraph.add_node "b" UidGraph.empty) ["a"; "b"];
    ]);
  Test("UidGraph.remove_node", [
      "remove one node", assert_set_eq (UidGraph.get_nodes @@ UidGraph.remove_node "b" ((UidS.of_list ["a"; "b"], UidPairS.empty))) ["a"];
      "remove last node", assert_set_eq (UidGraph.get_nodes @@ UidGraph.remove_node "a" ((UidS.of_list ["a"], UidPairS.empty))) [];
      "removes edges", assert_graph_eq (test_graph_with_edges) ["a"; "b"; "c"; "d"] [("a", "b"); ("b", "c")];
      "removes edges", assert_graph_eq (UidGraph.remove_node "a" test_graph_with_edges) ["b"; "c"; "d"] [("b", "c")];
      "removes edges (different node)", assert_graph_eq (UidGraph.remove_node "b" test_graph_with_edges) ["a"; "c"; "d"] [];
    ]);
  Test("UidGraph.add_edge", [
      "add first edge", assert_graph_eq (UidGraph.add_edge ("a", "b") test_graph) test_graph_nodes [("a", "b")];
      "try to add edge both ways", assert_graph_eq (UidGraph.add_edge ("b", "a") @@ UidGraph.add_edge ("a", "b") test_graph) test_graph_nodes [("a", "b")];
    ]);
  Test("UidGraph.fully_connected", [
      "with no nodes", assert_graph_eq (UidGraph.fully_connect (UidS.of_list []) test_graph) test_graph_nodes [];
      "with 1 node", assert_graph_eq (UidGraph.fully_connect (UidS.of_list ["a"]) test_graph) test_graph_nodes [];
      "with 2 nodes", assert_graph_eq (UidGraph.fully_connect (UidS.of_list ["a"; "b"]) test_graph) test_graph_nodes [("a", "b")];
      "with 3 nodes", assert_graph_eq (UidGraph.fully_connect (UidS.of_list ["a"; "b"; "c"]) test_graph) test_graph_nodes [("a", "b"); ("a", "c"); ("b", "c")];
    ]);
  Test("UidGraph.find_node_with_deg_less_than", [
      "on empty graph (deg 0)", assert_find_with_deg UidGraph.empty 0 [];
      "on empty graph (deg 100)", assert_find_with_deg UidGraph.empty 100 [];
      "on test graph (deg 100)", assert_find_with_deg test_graph_with_edges 100 ["a"; "b"; "c"; "d"];
      "on test graph (deg 2)", assert_find_with_deg test_graph_with_edges 2 ["a"; "c"; "d"];
      "on test graph (deg 1)", assert_find_with_deg test_graph_with_edges 1 ["d"];
      "on test graph (deg 0)", assert_find_with_deg test_graph_with_edges 0 [];
    ]);
  Test("UidGraph.find_node_with_deg_less_than_naive", [
      "on empty graph (deg 0)", assert_find_with_deg_naive UidGraph.empty 0 [];
      "on empty graph (deg 100)", assert_find_with_deg_naive UidGraph.empty 100 [];
      "on test graph (deg 100)", assert_find_with_deg_naive test_graph_with_edges 100 ["a"; "b"; "c"; "d"];
      "on test graph (deg 2)", assert_find_with_deg_naive test_graph_with_edges 2 ["a"; "c"; "d"];
      "on test graph (deg 1)", assert_find_with_deg_naive test_graph_with_edges 1 ["d"];
      "on test graph (deg 0)", assert_find_with_deg_naive test_graph_with_edges 0 [];
    ]);
  Test("UidGraph.get_neighbours", [
      "two neihgbours", assert_set_eq (UidGraph.get_neighbours "b" test_graph_with_edges) ["a"; "c"]; 
      "one neighbour", assert_set_eq (UidGraph.get_neighbours "a" test_graph_with_edges) ["b"]; 
      "no neighbours", assert_set_eq (UidGraph.get_neighbours "d" test_graph_with_edges) [];
    ]);
  Test("UidGraph.canonical_par", [
      "ordered", assert_eq (UidGraph.canonical_pair ("a", "b")) ("a", "b");
      "unordered", assert_eq (UidGraph.canonical_pair ("b", "a")) ("a", "b");
      "equal", assert_eq (UidGraph.canonical_pair ("a", "a")) ("a", "a");
    ]);
] 
