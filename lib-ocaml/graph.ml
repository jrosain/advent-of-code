module type Point = sig
  type t

  val compare : t -> t -> int
  val pr : t -> string
end

module MakeUF (V : Point) = struct
  type t = { parent: (V.t, V.t) Hashtbl.t; rank: (V.t, int) Hashtbl.t }

  let empty () : t =
    let big = 10000000 in
    { parent = Hashtbl.create big; rank = Hashtbl.create big }

  let rec find (x : V.t) (uf : t) : V.t =
    match Hashtbl.find_opt uf.parent x with
    | None -> Hashtbl.replace uf.parent x x; x
    | Some y -> if compare x y <> 0
               then
                 let py = find y uf in
                 Hashtbl.replace uf.parent x py; py
               else y

  let unite (x : V.t) (y : V.t) (uf : t) : t =
    let px = find x uf in
    let py = find y uf in
    if compare px py = 0 then uf
    else
      let rpx = match Hashtbl.find_opt uf.rank px with
        | Some n -> n
        | None -> 0 in
      let rpy = match Hashtbl.find_opt uf.rank py with
        | Some n -> n
        | None -> 0 in
      if rpx < rpy then (Hashtbl.replace uf.parent px py; uf)
      else if rpx > rpy then (Hashtbl.replace uf.parent py px; uf)
      else (Hashtbl.replace uf.parent py px;
            Hashtbl.replace uf.rank px (rpx + 1); uf)

  let connected (x : V.t) (y : V.t) (uf : t) : bool =
    compare (find x uf) (find y uf) = 0
end

module Make (V : Point) = struct
  module VSet = Set.Make(V)
  module VPair = struct
    type t = V.t * V.t

    let compare (x1,y1) (x2,y2) =
      let c = compare x1 x2 in
      if c = 0 then compare y1 y2
      else c
  end

  module VPairSet = Set.Make(VPair)

  type t = { edges: (V.t, V.t list) Hashtbl.t; cost: (V.t * V.t, int) Hashtbl.t }

  let empty () : t =
    let big = 10000000 in
    { edges=Hashtbl.create big; cost=Hashtbl.create big }


  let add_directed_edge ?cost (x : V.t) (y : V.t) (graph : t) : t =
    (match Hashtbl.find_opt graph.edges x with
    | None -> Hashtbl.add graph.edges x [y]
    | Some ls -> Hashtbl.replace graph.edges x (y :: ls));
    let cost = match cost with None -> 1 | Some n -> n in
    Hashtbl.replace graph.cost (x,y) cost;
    graph

  let add_undirected_edge ?cost (x : V.t) (y : V.t) (graph : t) : t =
    add_directed_edge ?cost y x @@ add_directed_edge ?cost x y graph

  let dfs (x : V.t) (graph : t) : VSet.t =
    let rec aux (seen : VSet.t) (x : V.t) : VSet.t =
      if VSet.mem x seen then seen
      else
        let seen = VSet.add x seen in
        match Hashtbl.find_opt graph.edges x with
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
    aux @@ List.of_seq (Hashtbl.to_seq_keys graph.edges)

  module UF = MakeUF(V)

  let kruskal ?steps (graph : t) : (V.t * V.t * int) list =
    let edges = List.of_seq (Hashtbl.to_seq graph.cost) in
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
    mst 0 VPairSet.empty edges (UF.empty())

  let vertices (g : t) : V.t list =
    List.of_seq @@ Hashtbl.to_seq_keys g.edges

  (* let parents (g : t) (x : V.t) : V.t list =
       let fold ls y =
         match Hashtbl.find_opt g.edges y with
         | None -> ls
         | Some l' -> if List.mem x l' then y :: ls else ls in
       List.fold_left fold [] (vertices g) *)

  let topo_sort (g : t) : V.t list =
    let seen = VSet.empty in
    let stack = ref [] in

    let rec dfs' (seen : VSet.t) (u: V.t) : VSet.t =
      let seen = VSet.add u seen in
      match Hashtbl.find_opt g.edges u with
      | None -> stack := u :: !stack; seen
      | Some ls ->
         let seen = List.fold_left (fun seen v -> if not @@ VSet.mem v seen then dfs' seen v else seen) seen ls in
         stack := u :: !stack; seen
    in

    let rec aux (seen : VSet.t) (vs : V.t list) : unit =
      match vs with
      | [] -> ()
      | v :: vs -> let seen = dfs' seen v in aux seen vs in

    aux seen (vertices g);
    !stack

  let count_paths_with_cstrs (to_visit : V.t list) (s : V.t) (d : V.t) (graph : t) : int =
    let dp = Hashtbl.create 100000 in
    let to_visit = VSet.of_list to_visit in
    let rec aux (x : V.t) : unit =
      if x = d then ()
      else
        match Hashtbl.find_opt dp x with
        | Some _ -> ()
        | None ->
           match Hashtbl.find_opt graph.edges x with
           | None -> Hashtbl.add dp x (0, VSet.empty)
           | Some next ->
              List.iter aux next;
              let ls = List.map (Hashtbl.find dp) next in
              (* Keep only the paths that have the maximal set of nodes to visit *)
              let ls' = List.filter_map (fun (n,s) -> if not @@ VSet.is_empty s then Some (n, s) else None) ls in
              let ls =
                if List.is_empty ls' then ls
                else
                  let max = List.fold_left (fun s (_,s') -> if VSet.subset s s' then s' else s) VSet.empty ls' in
                  List.filter (fun (_,s) -> VSet.equal s max) ls' in
              let s0 = if List.is_empty ls then VSet.empty else snd (List.hd ls) in
              let s0 = if VSet.mem x to_visit then VSet.add x s0 else s0 in
              (* Edge case: if the current vertex is s, we need to check that we have all the vertices *)
              let ls = if x = s then List.filter (fun (_,s) -> VSet.equal s to_visit) ls else ls in
              Hashtbl.add dp x (List.fold_left (+) 0 @@ List.map fst ls,s0) in
    Hashtbl.add dp d (1, VSet.empty); aux s;
    match Hashtbl.find_opt dp s with
    | None -> 0
    | Some (n,_) -> n

  let count_paths (s : V.t) (d : V.t) (graph : t) : int =
    count_paths_with_cstrs [] s d graph
end
