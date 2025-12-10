open Lib

let parse : string list -> Coord.t list =
  let fold l s =
    match String.split_on_char ',' s with
    | [ c ; r ] -> Coord.make (int_of_string c) (int_of_string r) :: l
    | _ -> failwith (Printf.sprintf "Illegal input: expected coordinate, got %s" s)
  in
  List.fold_left fold []

let solve (coords : (Coord.t * Coord.t) list) : int =
  let rec aux (acc : int) (coords : (Coord.t * Coord.t) list) : int =
    match coords with
    | [] -> acc
    | (c1,c2) :: cs ->
       let v = Coord.vec c1 c2 in
       let a = ((Coord.col v) + 1) * (abs (Coord.row v) + 1) in
       aux (max acc a) cs in
  aux 0 coords

let part1 (input : string list) : string =
  let coords = parse input in
  let coord_pairs =
    List.concat_map (fun c -> List.fold_left (fun l c' -> (c, c') :: l) [] coords) coords in
  string_of_int @@ solve coord_pairs

let is_valid_coord_pair (polygon : Geometry.polygon) (x,y : Coord.t * Coord.t) : bool =
  Geometry.is_rectangle_inside_polygon polygon @@ Geometry.make_rectangle x y

let part2 (input : string list) : string =
  let coords = parse input in
  let polygon = Geometry.make_polygon coords in
  let coord_pairs =
    List.concat_map (fun c -> List.fold_left (fun l c' -> (c, c') :: l) [] coords) coords in
  string_of_int @@ solve @@ List.filter (is_valid_coord_pair polygon) coord_pairs
