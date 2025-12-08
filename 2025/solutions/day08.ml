open Lib

module Graph = struct
  module type Point = sig
    type t

    val compare : t -> t -> int
    val pr : t -> string
  end

  module MakeUF (V : Point) = struct
    module VMap = Map.Make(V)

    type t = { parent: V.t VMap.t; rank: int VMap.t }

    let empty : t = { parent = VMap.empty; rank = VMap.empty }

    let rec find (x : V.t) (uf : t) : (V.t * t) =
      match VMap.find_opt x uf.parent with
      | None -> (x, { uf with parent=VMap.update x (fun _ -> Some x) uf.parent })
      | Some y -> if compare x y <> 0
                 then
                   let y, uf = find y uf in
                   (y, { uf with parent=VMap.update x (fun _ -> Some y) uf.parent })
                 else (y, uf)

    let unite (x : V.t) (y : V.t) (uf : t) : t =
      let px, uf = find x uf in
      let py, uf = find y uf in
      if compare px py = 0 then uf
      else
        let rank = VMap.update px (fun n -> Some (match n with None -> 0 | Some n -> n)) uf.rank in
        let rank = VMap.update py (fun n -> Some (match n with None -> 0 | Some n -> n)) rank in
        let rpx = Option.get (VMap.find_opt px rank) in
        let rpy = Option.get (VMap.find_opt py rank) in
        if rpx < rpy then { parent=VMap.update px (fun _ -> Some py) uf.parent; rank }
        else if rpx > rpy then { parent=VMap.update py (fun _ -> Some px) uf.parent; rank }
        else
          { parent=VMap.update py (fun _ -> Some px) uf.parent;
            rank=VMap.update px (fun _ -> Some (rpx + 1)) rank }

    let connected (x : V.t) (y : V.t) (uf : t) : bool =
      let px,_ = find x uf in
      let py,_ = find y uf in
      compare px py = 0
  end

  module Make (V : Point) = struct
    module VSet = Set.Make(V)
    module VMap = Map.Make(V)

    module VPair = struct
      type t = V.t * V.t

      let compare (x1,y1) (x2,y2) =
        let c = compare x1 x2 in
        if c = 0 then compare y1 y2
        else c
    end

    module VPairSet = Set.Make(VPair)
    module CostMap = Map.Make(VPair)

    type t = { edges: (V.t list) VMap.t; cost: int CostMap.t }

    let empty : t = { edges=VMap.empty; cost=CostMap.empty }

    let add_directed_edge ?cost (x : V.t) (y : V.t) (graph : t) : t =
      let edges = VMap.update x
                    (fun ls -> Some (match ls with None -> [y] | Some ls -> y :: ls))
                    graph.edges in
      let cost = match cost with None -> 1 | Some n -> n in
      let cost = CostMap.update (x, y)
                   (fun _ -> Some cost) graph.cost in
      { edges; cost }

    let add_undirected_edge ?cost (x : V.t) (y : V.t) (graph : t) : t =
      add_directed_edge ?cost y x @@ add_directed_edge ?cost x y graph

    let dfs (x : V.t) (graph : t) : VSet.t =
      let rec aux (seen : VSet.t) (x : V.t) : VSet.t =
        if VSet.mem x seen then seen
        else
          let seen = VSet.add x seen in
          match VMap.find_opt x graph.edges with
          | None -> seen
          | Some ls -> List.fold_left aux seen ls in
      aux VSet.empty x

    let connected_components (graph : t) : VSet.t list =
      let rec aux (vs : V.t list) : VSet.t list =
        match vs with
        | [] -> []
        | x :: xs ->
           let res = dfs x graph in
           let xs  = List.filter (fun y -> not @@ VSet.mem y res) xs in
           res :: aux xs in
      aux @@ fst (List.split @@ VMap.to_list graph.edges)

    module UF = MakeUF(V)

    let kruskal ?steps (graph : t) : (V.t * V.t * int) list =
      let edges = CostMap.to_list graph.cost in
      let edges = List.fast_sort (fun (_,c1) (_,c2) -> compare c1 c2) edges in
      let steps = match steps with
        | None -> max_int
        | Some steps -> steps in
      let rec mst (step : int) (seen_pairs : VPairSet.t) (edges : (VPair.t * int) list)
                (uf : UF.t) : (V.t * V.t * int) list =
        if step >= steps then []
        else
          match edges with
          | [] -> []
          | ((x,y),c) :: xs ->
             if VPairSet.mem (y,x) seen_pairs
             then mst step seen_pairs xs uf
             else
               let seen_pairs = VPairSet.add (x,y) seen_pairs in
               if not @@ UF.connected x y uf
               then (x, y, c) :: mst (step + 1) seen_pairs xs (UF.unite x y uf)
               else mst (step + 1) seen_pairs xs uf in
      mst 0 VPairSet.empty edges UF.empty
  end
end

module G = Graph.Make(Coord3D)

let parse : string list -> Coord3D.t list =
  let parse_coord s =
    let ls = String.split_on_char ',' s in
    match ls with
    | [ x ; y ; z ] -> Coord3D.make (int_of_string x) (int_of_string y) (int_of_string z)
    | _ -> failwith (Printf.sprintf "Illegal input: found %d integers instead of 3" (List.length ls)) in
  List.map parse_coord

module UF = Graph.MakeUF(Coord3D)

(* TODO: fix graph creation time *)
let part1 (input : string list) : string =
  let vertices = parse input in
  let edges = List.concat_map
                (fun c -> List.map (fun c' -> (c, c', Coord3D.dist c c'))
                         (List.filter (fun c' -> c <> c') vertices))
                vertices in
  let graph = List.fold_left
                (fun g (x,y,c) -> G.add_undirected_edge ~cost:c x y g) G.empty edges in
  let mst = G.kruskal ~steps:1000 graph in
  let graph' = List.fold_left
                 (fun g (x,y,c) -> G.add_undirected_edge ~cost:c x y g) G.empty mst in
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
                (fun g (x,y,c) -> G.add_undirected_edge ~cost:c x y g) G.empty edges in
  let (v1,v2,_) = List.hd (List.rev @@ G.kruskal graph) in
  string_of_int @@ Coord3D.x v1 * Coord3D.x v2
