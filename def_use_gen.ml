open Ast
open Graph

module G = UseGraph
(* Possible types of the nodes of the def-use graph *)
type node_type =
  | NVar of string
  | NExpr of expr
  | NRhs of rhs

(** Module modeling the template 
    of the def-use graph generator **)
module type DefUseGen = sig
  (* The type of the nodes of the def-use graph *)
  type node = G.node
  (* The type of the edges of the def-use graph *)
  type edge = G.edge
  (* The type of the def-use graph *)
  type graph = G.graph
  (** The type of the structure generated **)
  type generated

  (* Create a new structure *)
  val new_gen : unit -> generated

  (* Returns the graph present in the generated structure *)
  val get_graph: generated -> graph

  (* Returns the list of pairs present in the generated structure *)
  val get_pairs : generated -> (edge * (identifier * rhs)) list

  (* Creates, if not not already present, and returns a node *)
  val new_node: generated -> node_type -> node

  (** Given a command, a function to compute the cost of the arc generated,
      a function to compute the cost of the following commands in the block,
      and a flag signaling the protection vs Spectre1.1,
      returns the structure generated **)
  val populate_graph: cmd -> (cmd -> int -> int) -> (cmd -> int -> int) -> (bool) -> generated

  (** Given the structure generated so far, an expression and a cost,
      returns the new node generated **)
  val populate_graph_exp: generated -> expr -> int -> node

  (** Given the structure generated so far, a right hand-side and a cost,
      returns the new node generated **)
  val populate_graph_rhs: generated -> rhs -> int -> node

  val print_generated : generated -> unit

end

module HashTableGen : DefUseGen = struct

  type node = G.node
  type edge = G.edge
  type graph = G.graph
  (** The generated structure modeled as:
      an hashtable containing the nodes generated so far and their number identifier
      a list of node identifiers, variable identifiers and rhs (to be protected)
      a graph **)
  type generated = {
    hasht: (node_type, node) Hashtbl.t;
    mutable pairs: (edge * (identifier * rhs)) list;
    mutable g: graph;
  }

  let new_gen _ = 
    {
      hasht = Hashtbl.create 100;
      pairs = [];
      g = G.empty ();
    }

  let get_graph (gen: generated) : graph = gen.g

  let get_pairs (gen: generated) : (edge * (identifier * rhs)) list = gen.pairs

  let new_node (gen: generated) (node_name: node_type) : node =
    match Hashtbl.find_opt gen.hasht node_name with 
      | Some node -> node
      | None -> 
        let (g, node) = G.add gen.g in
        Hashtbl.add gen.hasht node_name node;
        gen.g <- g; node

  let rec populate_graph_exp (gen: generated) (e: expr) (cost: int) : node =
    match e with
      | Cst (v) ->
          new_node gen (NExpr e)
      | Var (id) ->
          new_node gen (NVar id)
      | BinOp (e1, e2, op) ->
          let n_node = new_node gen (NExpr e) in
          let n1 = populate_graph_exp gen e1 cost in
          let n2 = populate_graph_exp gen e2 cost in
          gen.g <- G.set_edge gen.g (n1, n_node) (-1); 
          gen.g <- G.set_edge gen.g (n2, n_node) (-1);
          n_node
      | InlineIf (e1, e2, e3) ->
          let n_node = new_node gen (NExpr e) in
          let n1 = populate_graph_exp gen e1 cost in
          let n2 = populate_graph_exp gen e2 cost in
          let n3 = populate_graph_exp gen e3 cost in
          gen.g <- G.set_edge gen.g (n1, n_node) (-1);
          gen.g <- G.set_edge gen.g (n2, n_node) (-1);
          gen.g <- G.set_edge gen.g (n3, n_node) (-1);
          n_node
      | Length(e1) -> 
          let n_node = new_node gen (NExpr e) in
          let n1 = populate_graph_exp gen e1 cost in
          gen.g <- G.set_edge gen.g (n1, n_node) (-1);
          n_node
      | Base(e1) -> 
          let n_node = new_node gen (NExpr e) in
          let n1 = populate_graph_exp gen e1 cost in
          gen.g <- G.set_edge gen.g (n1, n_node) (-1);
          n_node


  let rec populate_graph_rhs (gen: generated) (r: rhs) (cost: int) : node =
    let source = G.source gen.g in
    let sink = G.sink gen.g in
    match r with
      | Expr (e) ->
          let node_exp = populate_graph_exp gen e cost in
          node_exp
      | PtrRead (e, l) ->
          let r_node = new_node gen (NRhs r) in
          let n = populate_graph_exp gen e cost in
          gen.g <- G.set_edge gen.g (source, n) (-1);
          gen.g <- G.set_edge gen.g (n, sink) (-1);
          r_node
      | ArrayRead (a, e) ->
          let r_node = new_node gen (NRhs r) in
          let n = populate_graph_exp gen e cost in
          gen.g <- G.set_edge gen.g (source, r_node) (-1);
          gen.g <- G.set_edge gen.g (n, sink) (-1);
          r_node

  let populate_graph (c: cmd) (cost_f : cmd -> int -> int) (cost_r : cmd -> int -> int) (spectre: bool): generated = 
    let rec helper (gen: generated) (c: cmd) (cost: int) : generated =
      let sink = G.sink gen.g in
      match c with 
        | Fail -> gen
        | Skip -> gen
        | VarAssign (id, rhs) ->
            let id_node = new_node gen (NVar id) in
            let rhs_node = populate_graph_rhs gen rhs cost in
            gen.g <- G.set_edge gen.g (rhs_node, id_node) (cost_f c cost);
            gen.pairs <- ((rhs_node, id_node), (id, rhs)) :: gen.pairs;
            gen
        | PtrAssign (e1, e2, l) ->
            let ptr = populate_graph_exp gen e1 cost in
            let e = populate_graph_exp gen e2 cost in
            if spectre then 
              gen.g <- G.set_edge gen.g (e, sink) (cost_f c cost) 
            else ();
            gen.g <- G.set_edge gen.g (ptr, sink) (cost_f c cost);
            gen
        | ArrAssign (a, e1, e2) ->
            let index = populate_graph_exp gen e1 cost in
            let e = populate_graph_exp gen e2 cost in
            if spectre then (gen.g <- G.set_edge gen.g (e, sink) (cost_f c cost));
            gen.g <- G.set_edge gen.g (index, sink) (cost_f c cost);
            gen
        | Seq (c1, c2) -> 
            let gen = helper gen c1 (cost_r c cost) in
            let gen = helper gen c2 (cost_r c cost) in 
            gen
        | If (e, c1, c2) ->
            let gen = helper gen c1 (cost_r c cost) in
            let gen = helper gen c2 (cost_r c cost) in
            let cond_node = populate_graph_exp gen e cost in
            gen.g <- G.set_edge gen.g (cond_node, sink) (cost_f c cost);
            gen
        | While (e, c1) ->
            let gen = helper gen c1 (cost_r c cost) in
            let cond_node = populate_graph_exp gen e cost in 
            gen.g <- G.set_edge gen.g (cond_node, sink) (cost_f c cost);
            gen
        | Protect (id, p, rhs) ->
            let _ = populate_graph_rhs gen rhs cost in
            gen in
    let gen = new_gen () in
    helper gen c 1

  let print_generated (gen : generated) : unit =
    let strNt (nt : node_type) : string =
      match nt with
        | NVar(id) -> id
        | NExpr(_) -> "expr"
        | NRhs(_)  -> "rhs" in
    let print_hashtbl (h : (node_type, node) Hashtbl.t) : unit =
      Printf.printf "{\n";
      Hashtbl.iter (fun t n -> Printf.printf "\t(%s) -> %s\n"  (strNt t) (G.str_node n)) h;
      Printf.printf "}\n\n"; in
    let print_pairs (pls : (edge * (identifier * rhs)) list) : unit =
        Printf.printf "[ ";
        List.iter (fun ((n1, n2), (i, _)) -> Printf.printf "(%s -> %s, %s) " (G.str_node n1) (G.str_node n2) i) gen.pairs;
        Printf.printf "]\n\n"; in
    print_hashtbl gen.hasht;
    print_pairs gen.pairs;
    G.print_graph gen.g

end
