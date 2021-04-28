
module type Graph = sig

    type node
    type 'a graph
    type edge = node * node

    val empty : unit -> 'a graph

    val add : 'a graph -> ('a graph * node)

    val set_edge : 'a graph -> edge -> int -> 'a graph

    val capacity : 'a graph -> edge -> int

    val print_graph : 'a graph -> unit

    val print_nodes : node list -> unit

    val str_node : node -> string

    val source : 'a graph -> node

    val sink : 'a graph -> node

    val neighbours : 'a graph -> node -> node list

    val copy : 'a graph -> 'a graph

    (*
    val bfs : 'a graph -> (node -> unit) -> unit

    val bfs_fp : 'a graph -> ('a graph * int array)

    val max_flow : 'a graph -> int array -> int

    val update_graph : 'a graph -> int array -> int -> 'a graph * int
    *)

    val edmonds_karp : 'a graph -> ('a graph * node list)

    val filter_assoc : (node * 'b) list -> node list -> 'b list

end

module UseGraph : Graph = struct
    type node = int
    type matrix = int array array
    type 'a graph = {
        m : matrix
    }
    type edge = node * node

    let empty (() : unit) : 'a graph =
        { m = [| [| 0 ; 0 |] ; [| 0 ; 0 |] |] }

    let add (g : 'a graph) : ('a graph * node) =
        let append_node ( a : int array ) = Array.append a [| 0 |] in
        let osz (m : int array array) : int = Array.length m in
        let append_all (m : int array array) = Array.map append_node m in
        let add_array (m : int array array) = 
            Array.append m [| (Array.make ((osz g.m) + 1) 0) |] in
        ({ m = add_array (append_all g.m) }, (osz g.m) )

    let set_edge (g : 'a graph) ((n1, n2) : edge) (c : int) : 'a graph =
        g.m.(n1).(n2) <- c; g

    let capacity (g : 'a graph) ((n1, n2) : edge) : int =
        g.m.(n1).(n2)

    let print_graph (g : 'a graph) : unit = 
        let helper i = (fun j n ->
            if n <> 0 then
                Printf.printf "([%d -> %d]: %d) " i j n) in
        let outer_iter (i : int) (a : int array) : unit = Array.iteri (helper i) a in 
        Array.iteri outer_iter g.m;
        Printf.printf "\n"

    let print_nodes (nds : node list) : unit =
        Printf.printf "[ ";
        List.iter (fun v -> Printf.printf "%d " v) nds;
        Printf.printf "]\n"

    let str_node (n : node) : string =
        Printf.sprintf "%d" n

    let source (g : 'a graph) : node = 0

    let sink (g : 'a graph) : node = 1

    let neighbours (g : 'a graph) (n : node) : node list =
        let src = g.m.(n) in
        let folder (it : node list) (v : node) : node list = 
            if v >= 0 then v :: it else it in
        let mapier (i : int) (n : int) : int = 
            if n <> 0 then i else (-1) in
        Array.fold_left folder [] (Array.mapi mapier src)

    let copy (g : 'a graph) : 'a graph =
        let maper (a : int array) : int array = Array.copy a in
        let m' = Array.map maper g.m in
        { m = m' }

    let size (g : 'a graph) : int =
        Array.length g.m

    let bfs (g : 'a graph) (action : node -> unit) : unit =
        let color = Array.make (size g) 0 in
        let rec helper (stack : node list) (g : 'a graph) =
            match stack with
                | v :: vs -> 
                        if color.(v) == 1 then helper vs g
                        else
                            let nb = neighbours g v in
                            color.(v) <- 1;
                            action v;
                            helper (vs @ nb) g
                | [] -> () in
        helper [source g] g

    let bfs_sp (g : 'a graph) : ('a graph * node array) =
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
        let rec helper (stack : node list) (g : 'a graph) : ('a graph * node array) =
            match stack with
                | v :: vs -> 
                        let stack' = iter_nb v (neighbours g v) vs in
                        helper stack' g
                | [] -> (g, pred) in
        helper [source g] g


    let (<?) (a : int) (b : int) : bool =
        match a, b with
            | -1, -1  -> false
            | -1,  _  -> false
            | _ , -1  -> true
            | _ ,  _  -> (a < b)

    let (-?) (a:int) (b:int) : int =
        match a, b with
            | -1, -1  -> -1 
            | -1,  _  -> -1 
            | _ , -1  -> -1 
            | _ ,  _  -> (a - b)


    let max_flow (g : 'a graph) (pred: node array) : int =
        let rec folder (max : int) ((v1, v2) : (node * node)) : int =
            if v1 = (-1) then max
            else 
                let cap = capacity g (v1, v2) in
                let max' = if cap <? max then cap else max in
                folder max' (pred.(v1), v1) in
        let fe = (pred.(sink g), sink g) in
        folder (capacity g fe) fe

    let update_graph (g : 'a graph) (pred : node array) (max : int) : ('a graph * node) =
        let rec updater (g : 'a graph) ((v1, v2) : (node * node)) (cut : node) : ('a graph * node) =
            if v1 = (-1) then (g, cut) 
            else 
                let cap = capacity g (v1, v2) in
                let g' = set_edge g (v1, v2) (cap -? max) in
                let cut' = if (cap -? max) = 0 && cut = (-1) then v2 else cut in
                updater g' (pred.(v1), v1) cut' in
        updater g (pred.(sink g), sink g) (-1)

    let edmonds_karp (g : 'a graph) : ('a graph * node list) =
        let g' = copy g in
        let rec add_absent (cuts : node list) (c : node) : (node list) =
            match cuts with
                | v :: vs -> 
                        if v = c then v :: vs
                        else v :: (add_absent vs c)
                | [] -> [ c ] in
        let rec helper (g : 'a graph) (cuts : node list) : ('a graph * node list) = 
            let (g, pred) = bfs_sp g in
            match pred.(sink g) with
                | (-1) -> (g, cuts)
                | _    -> let max = max_flow g pred in
                          let (g, c) = update_graph g pred max in
                          helper g (add_absent cuts c) in
        helper g' []

    let filter_assoc (ls : (node * 'b) list) (nodes : node list) : ('b list) =
        let ls_sorter ((n1, _) : int * 'b) ((n2, _) : int * 'b) : int = n1 - n2 in
        let nodes_sorter (n1 : int) (n2 : int) : int = n1 - n2 in
        let ls' = List.sort ls_sorter ls in
        let nodes' = List.sort nodes_sorter nodes in
        let rec helper (ls : (node * 'b) list) (nodes : node list) (acc : 'b list) : ('b list) =
            match ls, nodes with
                | (v, l) :: ls', n :: nodes' -> 
                        if n = v then helper ls' nodes' (l :: acc)
                        else 
                            if n > v then helper ls' nodes acc
                            else helper ls nodes' acc
                | [], _ -> acc
                | _, [] -> acc in
        helper ls' nodes' []
                                                
end


