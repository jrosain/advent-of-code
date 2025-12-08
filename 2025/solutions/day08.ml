open Lib

module G = Graph.Make(Coord3D)

let parse : string list -> Coord3D.t list =
  let parse_coord s =
    let ls = String.split_on_char ',' s in
    match ls with
    | [ x ; y ; z ] -> Coord3D.make (int_of_string x) (int_of_string y) (int_of_string z)
    | _ -> failwith (Printf.sprintf "Illegal input: found %d integers instead of 3" (List.length ls)) in
  List.map parse_coord

module UF = Graph.MakeUF(Coord3D)

let part1 (input : string list) : string =
  let vertices = parse input in
  let edges = List.concat_map
                (fun c -> List.map (fun c' -> (c, c', Coord3D.dist c c'))
                         (List.filter (fun c' -> c <> c') vertices))
                vertices in
  let graph = List.fold_left (fun g (x,y,c) -> G.add_directed_edge ~cost:c x y g) (G.empty()) edges in
  let mst = G.kruskal ~steps:1000 graph in
  let graph' = List.fold_left (fun g (x,y,_) -> G.add_undirected_edge x y g) (G.empty()) mst in
  let cc = List.fast_sort (fun x y -> compare y x) @@
             List.map (fun s -> G.VSet.cardinal s) (G.connected_components graph') in
  let cc = List.take 3 cc in
  string_of_int @@ List.fold_left ( * ) 1 cc

let part2 (input : string list) : string =
  let vertices = parse input in
  let edges = List.concat_map
                (fun c -> List.map (fun c' -> (c, c', Coord3D.dist c c'))
                         (List.filter (fun c' -> c <> c') vertices))
                vertices in
  let graph = List.fold_left
                (fun g (x,y,c) -> G.add_directed_edge ~cost:c x y g) (G.empty()) edges in
  let (v1,v2,_) = List.hd (List.rev @@ G.kruskal graph) in
  string_of_int @@ Coord3D.x v1 * Coord3D.x v2
