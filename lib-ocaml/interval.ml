exception Unmergeable

type t = { st: int; ed: int }

let make (st : int) (ed : int) : t = { st; ed }

let mem (x : int) (i : t) : bool =
  i.st <= x && x <= i.ed

let len (i : t) : int = i.ed - i.st + 1

let pr (i : t) : string =
  Printf.sprintf "[%d, %d]" i.st i.ed

let overlap (i : t) (i' : t) : bool =
  (i.ed >= i'.st && i.st <= i'.ed) ||
    (i'.ed >= i.st && i'.st <= i.ed)

let merge (i : t) (i' : t) : t =
  if overlap i i'
  then { st=min i.st i'.st; ed=max i.ed i'.ed }
  else raise Unmergeable
