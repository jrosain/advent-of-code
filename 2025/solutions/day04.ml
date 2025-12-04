open Lib

let parse : string list -> Coord.Set.t =
  let parse_row (row : int) (col : int) : string -> Coord.Set.t =
    let c = ref col in
    let fold s x =
      let s = if x = '@' then Coord.Set.add (Coord.make !c row) s
              else s in
      c := !c + 1; s
    in
    String.fold_left fold Coord.Set.empty in
  let rec parse_aux (row : int) (l : string list) : Coord.Set.t =
    match l with
    | [] -> Coord.Set.empty
    | x :: xs -> Coord.Set.union (parse_row row 0 x) (parse_aux (row + 1) xs) in
  parse_aux 0

let make_coords_map (coords : Coord.Set.t) : int Coord.Map.t =
  let init_map = Coord.Set.fold (fun c m -> Coord.Map.update c (fun _ -> Some 0) m)
                   coords Coord.Map.empty in
  let fold c m =
    List.fold_left
      (fun m c' ->
        if Coord.Set.mem c' coords
        then Coord.Map.update c (fun n -> Some (match n with None -> 1 | Some n -> n + 1)) m
        else m) m
      (Coord.neighbors c) in
  Coord.Set.fold fold coords init_map

let accessible (m : int Coord.Map.t) : Coord.t list =
  fst @@ List.split @@ Coord.Map.to_list @@ Coord.Map.filter (fun _ n -> n < 4) m

let part1 (input : string list) : string =
  let coords = parse input in
  let map = make_coords_map coords in
  string_of_int @@ List.length (accessible map)

let remove (m : int Coord.Map.t) (accessible : Coord.t list) : int Coord.Map.t =
  let fold m c =
    let fold' m c' = Coord.Map.update c' (fun n -> Option.bind n (fun m -> Some (m - 1))) m in
    let m = List.fold_left fold' m (Coord.neighbors c) in
    Coord.Map.remove c m in
  List.fold_left fold m accessible

let part2 (input : string list) : string =
  let coords = parse input in
  let map = make_coords_map coords in
  let rec solve (m : int Coord.Map.t) : int =
    let acc = accessible m in
    match acc with
    | [] -> 0
    | _  -> List.length acc +
             solve (remove m acc) in
  string_of_int @@ solve map
