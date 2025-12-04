type t

val make : int -> int -> t

val neighbors : ?include_diagonals:bool -> t -> t list

module Set : sig include Set.S with type elt = t end
module Map : sig include Map.S with type key = t end
