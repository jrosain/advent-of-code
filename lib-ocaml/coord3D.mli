
type t

val make : int -> int -> int -> t

val compare : t -> t -> int

(** Returns the squared euclidean distance between vertices *)
val dist : t -> t -> int

val pr : t -> string

val x : t -> int
