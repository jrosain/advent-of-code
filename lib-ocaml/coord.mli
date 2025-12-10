type t

val make : int -> int -> t

val col : t -> int
val row : t -> int

val left : t -> t

val right : t -> t

val down : t -> t

(** Squared value of euclidean distance between two coords *)
val euclidean_dist : t -> t -> int

(** Checks if the coordinate is in the same row or below of the given one *)
val below : t -> int -> bool

(** The 2D vector between two coordinates *)
val vec : t -> t -> t

(** Dot product between two vectors *)
val dot : t -> t -> int

(** Cross product between two vectors *)
val cross : t -> t -> int

val add : t -> t -> t
val mul : int -> t -> t

(** Gets all the neighbors of a coordinate. Coords in diagonal are included
    by default. *)
val neighbors : ?include_diagonals:bool -> t -> t list

module Set : sig include Set.S with type elt = t end
module Map : sig include Map.S with type key = t end

(** Gets all the coordinates of the character in the grid. *)
val from_grid : char -> string list -> Set.t

val pr : t -> string
