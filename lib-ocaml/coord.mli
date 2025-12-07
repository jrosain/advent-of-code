type t

val make : int -> int -> t

val left : t -> t

val right : t -> t

val down : t -> t

(** Checks if the coordinate is in the same row or below of the given one *)
val below : t -> int -> bool

(** Gets all the neighbors of a coordinate. Coords in diagonal are included
    by default. *)
val neighbors : ?include_diagonals:bool -> t -> t list

module Set : sig include Set.S with type elt = t end
module Map : sig include Map.S with type key = t end

(** Gets all the coordinates of the character in the grid. *)
val from_grid : char -> string list -> Set.t
