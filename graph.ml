
module type Graph = sig

    type node
    type 'a graph
    type edge = node * node

    val empty : unit -> 'a graph

    val add : 'a graph -> ('a graph * node)

    val set_edge : 'a graph -> edge -> int -> 'a graph

    val capacity : 'a graph -> edge -> int

    val print_graph : 'a graph -> unit

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
        { m = [| src ; snk |] };;

    let add (g : 'a graph) : ('a graph * node) =
        let append_node ( a : int array ) = Array.append a [| 0 |] in
        let osz (m : int array array) : int = Array.length m in
        let append_all (m : int array array) = Array.map append_node m in
        let add_array (m : int array array) = Array.append m [| (Array.make ((osz g.m) + 1) 0) |] in
        ({ m = add_array (append_all g.m) }, (osz g.m) );;

    let set_edge (g : 'a graph) ((n1, n2) : edge) (c : int) : 'a graph =
        g.m.(n1).(n2) <- c; g;;

    let capacity (g : 'a graph) ((n1, n2) : edge) : int =
        g.m.(n1).(n2);;

    let print_graph (g : 'a graph) : unit = 
        let helper i = (fun j n ->
            if n <> 0 then
                Printf.printf "([%d;%d]: %d) " i j n) in
        let outer_iter (i : int) (a : int array) : unit = Array.iteri (helper i) a in 
        Array.iteri outer_iter g.m;;

end



        

