open Ast
open Graph

(* TODO add function for different costs for branches *)

(* Possible types of the nodes of the def-use graph *)
type node_type =
  | NVar of string
  | NExpr of expr
  | NRhs of rhs

(* Abstract module for the def-use graph *)
module type DefUseGen = sig
  type node
  type graph
  type generated

  (* create a new generator *)
  val new_gen : generated

  (* returns the graph generated *)
  val get_graph: generated -> graph

  (* creates, if not not already present, and returns a node*)
  val new_node: t -> node_type -> node

  val populate_graph: t -> cmd -> unit

  val populate_graph_exp: generated -> node_type -> node

  val populate_graph_rhs: generated -> node_type -> node
end

module HashTableGen : DefUseGen

  type node = Graph.node
  type graph = string Graph.graph 
  type generated = {
    hasht: (node_type, node) Hashtbl.t;
    pairs: (node, identifier) list;
    mutable g: graph;
  }

  let new_gen = 
    {
      hasht: Hashtbl.create 100;
      pairs: [];
      g: G.empty ();
    }

  let get_graph (gen: generated) : graph = gen.g

  let new_node (gen: generated) (node_name: node_type) : node =
    match Hashtbl.find_opt gen.hasht node_name with 
      | Some node -> node
      | None -> 
        let (g, node) = G.add gen.g node_name in
        Hashtbl.add gen.hasht node_name node;
        gen.g <- g; node

  let rec populate_graph_exp (gen: generated) (e: expr) : node =
    let source = G.source gen.g in
    let sink = G.sink gen.g in
    match e with
      | Cst (v) ->
        new_node gen (NExpr e)
      | Var (id) ->
        new_node gen (NVar e)
      | BinOp (e1, e2, op) ->
        let n_node = new_node gen (NExpr e) in
        let b1 = populate_graph_exp gen e1 in
        let b2 = populate_graph_exp gen e2 in
        gen.g <- G.set_edge gen.g (b1, n_node) 1;
        gen.g <- G.set_edge gen.g (b2, n_node) 1;
        n_node
      | InlineIf (e1, e2, e3) ->
        let n_node = new_node gen (NExpr e) in
        let n1 = populate_graph_exp gen e1 in
        let n2 = populate_graph_exp gen e2 in
        let n3 = populate_graph_exp gen e3 in
        gen.g <- G.set_edge gen.g (n1, n_node) 1;
        gen.g <- G.set_edge gen.g (n2, n_node) 1;
        gen.g <- G.set_edge gen.g (n3, n_node) 1;
        n_node
  
  let rec populate_graph_rhs (gen: generated) (r: rhs) : node =
    let source = G.source gen.g in
    let sink = G.sink gen.g in
    match r with
      | Expr (e) ->
        let node_exp = populate_graph_exp gen e in
        node_exp
      | PtrRead (e, l) ->
        let r_node = new_node gen (NRhs r) in
        let n = populate_graph_exp gen e in
        gen.g <- G.set_edge gen.g (source, n) -1;
        gen.g <- G.set_edge gen.g (n, sink) -1;
        r_node
      | ArrayRead (a, e) ->
        let r_node = new_node gen (NRhs r) in
        let n = populate_graph_exp gen e in
        gen.g <- G.set_edge gen.g (source, r_node) -1;
        gen.g <- G.set_edge gen.g (n, sink) -1;
        r_node

  let rec populate_graph (gen: generated) (c: cmd) : unit = 
    let sink = G.sink gen.g in
    match c with 
      | Fail -> ()
      | Skip -> ()
      | VarAssign (id, rhs) ->
        let id_node = new_node gen (NVar id) in
        let rhs_node = populate_graph_rhs gen rhs in
        gen.g <- G.set_edge gen.g (rhs_node, id_node) 1;
        gen.pairs <- (id_node, id) :: gen.pairs
      | PtrAssign (e1, e2, l) ->
        let ptr = populate_graph_exp gen e1 in
        let _ = populate_graph_exp gen e2 in
        gen.g <- G.set_edge gen.g (ptr, sink) -1;
      | ArrAssign (a, e1, e2) ->
        let _ = populate_graph_exp gen e1 in
        let index = populate_graph_exp gen e2 in
        gen.g <- G.set_edge gen.g (index, sink) -1;
      | Seq (c1, c2) -> 
        populate_graph gen c1;
        populate_graph gen c2
      | If (e, c1, c2) ->
        let _ = populate_graph gen c1 in
        let _ = populate_graph gen c2 in
        let cond_node = populate_graph_exp gen e in
        gen.g <- G.set_edge gen.g (cond_node, sink) -1;
      | While (e, c) ->
        let _ = populate_graph gen c in
        let cond_node = populate_graph_exp gen e in 
        gen.g <- G.set_edge gen.g (cond_node, sink) -1;
      | Protect (id, p, rhs) ->
        populate_graph_rhs gen rhs
      end

