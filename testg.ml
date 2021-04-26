open Graph_ext

module G = UseGraph;;

let g = G.empty ();;
let (g, n2) = G.add g;;
let (g, n3) = G.add g;;
let (g, n4) = G.add g;;
let (g, n5) = G.add g;;
let g = G.set_edge g (G.source g, n2) 8;;
let g = G.set_edge g (G.source g, n4) 4;;
let g = G.set_edge g (n2, n3) 1;;
let g = G.set_edge g (n4, n5) 7;;
let g = G.set_edge g (n5, n3) 1;;
let g = G.set_edge g (n3, G.sink g) 3;;

G.print_graph g;;
let (g, cut) = G.edmonds_karp g;;
G.print_graph g;;

let g = G.empty ();;
let (g, n2) = G.add g;;
let (g, n3) = G.add g;;
let (g, n4) = G.add g;;
let (g, n5) = G.add g;;
let (g, n6) = G.add g;;
let g = G.set_edge g (G.source g, n2) 2;;
let g = G.set_edge g (n2, n3) 3;;
let g = G.set_edge g (n2, n4) 4;;
let g = G.set_edge g (n3, n6) 1;;
let g = G.set_edge g (n4, n5) 7;;
let g = G.set_edge g (n5, n6) 1;;
let g = G.set_edge g (n6, G.sink g) 10;;

G.print_graph g;;
let (g, cut) = G.edmonds_karp g;;
G.print_graph g;;

