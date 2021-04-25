open Graph

module G = UseGraph;;

let g = G.empty () in
let (g, n1) = G.add g in
let (g, n2) = G.add g in
let (g, n3) = G.add g in
let g = G.set_edge g (n1, n2) 3 in
let g = G.set_edge g (n3, n2) 1 in
G.print_graph g
