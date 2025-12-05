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

let rec merge_step (is : t list) : t list =
  match is with
  | [] -> []
  | i :: is ->
     let is' = List.filter (overlap i) is in
     if List.is_empty is' then i :: merge_step is
     else
       let i0 = List.fold_left merge i is' in
       let is'' = List.filter (fun i -> not @@ List.mem i is') is in
       i0 :: merge_step is''

let rec merge_all (is : t list) : t list =
  let is' = merge_step is in
  if is' = is then is
  else merge_all is'
