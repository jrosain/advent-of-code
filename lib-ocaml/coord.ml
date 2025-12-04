type t = { col: int ; row: int }

let make (col : int) (row : int) : t = { col; row }

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
