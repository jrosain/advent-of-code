
type solution_func = (string list -> string) * (string list -> string)

type t = ((int, solution_func) Hashtbl.t)

let register : t = Hashtbl.create 30

let add ~(day: int) (f: solution_func) : unit =
  Hashtbl.add register day f

let get ~(day : int) : solution_func option =
  Hashtbl.find_opt register day
