
module type Graph = sig

    type node
    type 'a graph
    type edge = node * node

    val empty : unit -> 'a graph

    val add : 'a graph -> ('a graph * node)

    val set_edge : 'a graph -> edge -> int -> 'a graph

    val capacity : 'a graph -> edge -> int

    val print_graph : 'a graph -> unit

    val str_node : node -> string

    val source : 'a graph -> node

    val sink : 'a graph -> node

    val neighbours : 'a graph -> node -> node list

    val bfs : 'a graph -> (node -> unit) -> unit
end

module UseGraph : Graph = struct
    type node = int
    type matrix = int array array
    type 'a graph = {
        m : matrix
    }
    type edge = node * node

    let empty (() : unit) : 'a graph =
        let src = [| 0 ; 0 |] in
        let snk = [| 0 ; 0 |] in
        { m = [| src ; snk |] }

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
                Printf.printf "([%d;%d]: %d) " i j n) in
        let outer_iter (i : int) (a : int array) : unit = Array.iteri (helper i) a in 
        Array.iteri outer_iter g.m

    let str_node (n : node) : string =
        Printf.sprintf "%d" n

    let source (g : 'a graph) : node = 0

    let sink (g : 'a graph) : node = 1

    let neighbours (g : 'a graph) (n : node) : node list =
        let src = g.m.(n) in
        let folder (it : node list) (v : node) : node list = 
            if v >= 0 then v :: it else it in
        let mapier (i : int) (n : int) : int = 
            if n > 0 then i else (-1) in
        Array.fold_left folder [] (Array.mapi mapier src)

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

end


