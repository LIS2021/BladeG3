(** Module modeling the template of a graph structure
    along with various utilities functions
    and the edmond karp algorithm **)
module type Graph = sig
  type node;;
  type graph;;
  type edge = node * node;;

  (* Returns a new empty graph *)
  val empty : unit -> graph;;

  (** Given a graph
      returns a pair composed of the new graph
      and the identifier of the node added**)
  val add : graph -> (graph * node);;

  (** Given a graph, an edge and an integer,
      returns the graph updated with the edge capacity
      set to the given integer **)
  val set_edge : graph -> edge -> int -> graph;;

  (** Given a graph and an edge
      returns the capacity of the given edge **)
  val capacity : graph -> edge -> int;;

  val print_graph : graph -> unit;;

  val print_nodes : node list -> unit;;

  val str_node : node -> string;;

  val source : graph -> node;;

  val sink : graph -> node;;

  (** Given a graph and a node identifier
      returns a list of node identifiers,
      being neighbours of the given node**)
  val neighbours : graph -> node -> node list;;

  val copy : graph -> graph;;

  (** Utilities function used internally in the edmonds_karp function **)
  (**
  val bfs : graph -> (node -> unit) -> unit

  val bfs_fp : graph -> (graph * int array)

  val max_flow : graph -> int array -> int

  val update_graph : graph -> int array -> int -> graph * int
  **)

  (** Given a graph
      returns a pair composed of a graph
      and a list of node representing the cut list
      identified by the edmond karp algorithm **)
  val edmonds_karp : graph -> (graph * edge list);;

  val filter_assoc : (edge * 'b) list -> edge list -> 'b list;;
end;;

module UseGraph : Graph = struct
  type node = int;;
  type matrix = int array array;;
  type graph = { m : matrix };;
  type edge = node * node;;

  let empty (() : unit) : graph = { m = [| [| 0 ; 0 |] ; [| 0 ; 0 |] |] };;

  let add (g : graph) : (graph * node) =
    let append_node ( a : int array ) = Array.append a [| 0 |] in
    let osz (m : int array array) : int = Array.length m in
    let append_all (m : int array array) = Array.map append_node m in
    let add_array (m : int array array) = Array.append m [| (Array.make ((osz g.m) + 1) 0) |] in
    ({ m = add_array (append_all g.m) }, (osz g.m));;

  let set_edge (g : graph) ((n1, n2) : edge) (c : int) : graph =
    g.m.(n1).(n2) <- c; g;;

  let capacity (g : graph) ((n1, n2) : edge) : int =
    g.m.(n1).(n2);;

  let print_graph (g : graph) : unit =
    let helper i = fun j n -> if n <> 0 then Printf.printf "([%d -> %d]: %d) " i j n in
    let outer_iter (i : int) (a : int array) : unit = Array.iteri (helper i) a in
    Array.iteri outer_iter g.m;
    Printf.printf "\n";;

  let print_nodes (nds : node list) : unit =
    Printf.printf "[ ";
    List.iter (Printf.printf "%d ") nds;
    Printf.printf "]\n";;

  let str_node (n : node) : string =
    Printf.sprintf "%d" n;;

  let source (g : graph) : node = 0;;

  let sink (g : graph) : node = 1;;

  let neighbours (g : graph) (n : node) : node list =
    let src = g.m.(n) in
    let folder (it : node list) (v : node) : node list =
      if v >= 0 then v :: it else it in
    let mapier (i : int) (n : int) : int = if n <> 0 then i else (-1) in
    Array.fold_left folder [] (Array.mapi mapier src);;

  let copy (g : graph) : graph =
    let maper (a : int array) : int array = Array.copy a in
    let m' = Array.map maper g.m in
    { m = m' };;

  let size (g : graph) : int =
    Array.length g.m

  let bfs (g : graph) (action : node -> unit) : unit =
    let color = Array.make (size g) 0 in
    let rec helper (stack : node list) (g : graph) =
      match stack with
        | v :: vs ->
          if color.(v) == 1 then
            helper vs g
          else
            let nb = neighbours g v in
            color.(v) <- 1;
            action v;
            helper (vs @ nb) g
        | [] -> () in
    helper [source g] g;;

  let bfs_sp (g : graph) : (graph * node array) =
    let pred = Array.make (size g) (-1) in
    let rec iter_nb (v : node) (nbs : node list) (stack : node list) : node list =
      match nbs with
        | nb :: nbs' ->
            if pred.(nb) = (-1) && nb <> 0 then
              (pred.(nb) <- v;
              iter_nb v nbs' (stack @ [nb]))
            else
              iter_nb v nbs' stack
        | [] -> stack in
    let rec helper (stack : node list) (g : graph) : (graph * node array) =
      match stack with
        | v :: vs ->
            let stack' = iter_nb v (neighbours g v) vs in
            helper stack' g
        | [] -> (g, pred) in
    helper [source g] g;;


  let (<?) (a : int) (b : int) : bool =
    match a, b with
      | -1, -1  -> false
      | -1,  _  -> false
      | _ , -1  -> true
      | _ ,  _  -> a < b;;

  let (-?) (a : int) (b : int) : int =
    match a, b with
      | -1, -1  -> -1
      | -1,  _  -> -1
      | _ , -1  -> -1
      | _ ,  _  -> a - b;;


  let max_flow (g : graph) (pred: node array) : int =
    let rec folder (max : int) ((v1, v2) : (node * node)) : int =
      if v1 = (-1) then
        max
      else
          let cap = capacity g (v1, v2) in
          let max' = if cap <? max then cap else max in
          folder max' (pred.(v1), v1) in
    let fe = (pred.(sink g), sink g) in
    folder (capacity g fe) fe;;

  let update_graph (g : graph) (pred : node array) (max : int) : (graph * edge) =
    let rec updater (g : graph) ((v1, v2) : (node * node)) (cut : edge) : (graph * edge) =
      if v1 = (-1) then
        (g, cut)
      else
        let cap = capacity g (v1, v2) in
        let g' = set_edge g (v1, v2) (cap -? max) in
        let cut' = if (cap -? max) = 0 && cut = (-1, -1) then (v1, v2) else cut in
        updater g' (pred.(v1), v1) cut' in
    updater g (pred.(sink g), sink g) (-1, -1);;

  let edmonds_karp (g : graph) : (graph * edge list) =
    let g' = copy g in
    let rec add_absent (cuts : edge list) (e : edge) : (edge list) =
      match cuts with
        | v :: vs ->
            if v = e then
              v :: vs
            else
              v :: (add_absent vs e)
        | [] -> [ e ] in
    let rec helper (g : graph) (cuts : edge list) : (graph * edge list) =
      let (g, pred) = bfs_sp g in
      match pred.(sink g) with
        | (-1) -> (g, cuts)
        | _    -> let max = max_flow g pred in
                  let (g, e) = update_graph g pred max in
                  helper g (add_absent cuts e) in
    helper g' [];;

  let filter_assoc (ls : (edge * 'b) list) (edges : edge list) : ('b list) =
    let ls_sorter ((e1, _) : edge * 'b) ((e2, _) : edge * 'b) : int = compare e1 e2 in
    let edges_sorter = compare in
    let ls' = List.sort ls_sorter ls in
    let edges' = List.sort edges_sorter edges in
    let rec helper (ls : (edge * 'b) list) (edges : edge list) (acc : 'b list) : ('b list) =
      match ls, edges with
        | (v, l) :: ls', n :: edges' ->
            if n = v then
              helper ls' edges' (l :: acc)
            else
              if n > v then helper ls' edges acc else helper ls edges' acc
        | [], _ -> acc
        | _, [] -> acc in
    helper ls' edges' [];;
end;;
