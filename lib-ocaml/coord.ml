type t = { col: int ; row: int }

let make (col : int) (row : int) : t = { col; row }

let col (c : t) : int = c.col
let row (c : t) : int = c.row

let left (coord : t) : t =
  { col=coord.col - 1; row=coord.row }

let right (coord : t) : t =
  { col=coord.col + 1; row=coord.row }

let down (coord : t) : t =
  { col=coord.col; row=coord.row + 1 }

let euclidean_dist (c1 : t) (c2 : t) : int =
  let col = c1.col - c2.col in
  let row = c1.row - c2.row in
  col*col + row*row

let below (coord : t) (lim : int) : bool =
  coord.row >= lim

let vec (c1 : t) (c2 : t) : t =
  make (c1.col - c2.col) (c1.row - c2.row)

let dot (v1 : t) (v2 : t) : int =
  v1.col * v2.col + v1.row * v2.row

let cross (v1 : t) (v2 : t) : int =
  v1.col * v2.row - v1.row * v2.col

let add (v1 : t) (v2 : t) : t =
  { col=v1.col + v2.col; row=v1.row + v2.row }

let mul (k : int) (v : t) : t =
  { col=k*v.col; row=k*v.row }

let neighbors ?(include_diagonals=true) (coord : t) : t list =
  let neighs = [
      { col=coord.col - 1; row=coord.row };
      { col=coord.col + 1; row=coord.row };
      { col=coord.col; row=coord.row - 1 };
      { col=coord.col; row=coord.row + 1 };
    ] in
  if not include_diagonals then neighs
  else List.append neighs [
           { col=coord.col - 1; row=coord.row - 1 };
           { col=coord.col - 1; row=coord.row + 1 };
           { col=coord.col + 1; row=coord.row - 1 };
           { col=coord.col + 1; row=coord.row + 1 };
         ]

module OrderedCoord = struct
  type nonrec t = t

  let compare c1 c2 =
    let r = compare c1.col c2.col in
    if r = 0 then compare c1.row c2.row
    else r
end

module Set = Set.Make(OrderedCoord)
module Map = Map.Make(OrderedCoord)

let from_grid (c : char) : string list -> Set.t =
  let parse_row (row : int) (s : string) : Set.t =
    let fold (s,col) x =
      let s = if x = c then Set.add (make col row) s
              else s in
      (s,col+1)
    in
    fst (String.fold_left fold (Set.empty, 0) s) in
  let rec parse_aux (row : int) (l : string list) : Set.t =
    match l with
    | [] -> Set.empty
    | x :: xs -> Set.union (parse_row row x) (parse_aux (row + 1) xs) in
  parse_aux 0

let pr (c : t) : string =
  Printf.sprintf "(%d, %d)" c.col c.row
