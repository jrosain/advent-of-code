exception Unmergeable

type t

val make : int -> int -> t

val mem : int -> t -> bool

val len : t -> int

val pr : t -> string

val overlap : t -> t -> bool

val merge : t -> t -> t

val merge_all : t list -> t list
