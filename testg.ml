open Graph_ext

module G = UseGraph;;

let g = G.empty ();;
let (g, n1) = G.add g;;
let (g, n2) = G.add g;;
let (g, n3) = G.add g;;
let g = G.set_edge g (n1, n2) 3;;
let g = G.set_edge g (n3, n2) 1;;
let g = G.set_edge g (G.source g, n1) 1;;
let g = G.set_edge g (G.source g, n3) 1;;
let g = G.set_edge g (n2, G.sink g) 3;;

G.print_graph g;;

let print_node (n : G.node) : unit =
    Printf.printf "%s " (G.str_node n);;

List.iter print_node (G.neighbours g (G.source g));;
List.iter print_node (G.neighbours g (G.sink g));;
List.iter print_node (G.neighbours g n1);;
List.iter print_node (G.neighbours g n2);;
List.iter print_node (G.neighbours g n3);;

G.bfs g (fun v -> Printf.printf "%s\n" (G.str_node v));;
