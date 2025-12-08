module type Point = sig
  type t

  val compare : t -> t -> int
  val pr : t -> string
end

module MakeUF (V : Point) : sig
  type t

  val empty : t

  val find : V.t -> t -> V.t * t
  val unite : V.t -> V.t -> t -> t
  val connected : V.t -> V.t -> t -> bool
end

module Make (V : Point) : sig
  type t

  module VSet : sig include Set.S with type elt = V.t end
  module VMap : sig include Map.S with type key = V.t end

  val empty : t

  val add_directed_edge : ?cost:int -> V.t -> V.t -> t -> t
  val add_undirected_edge : ?cost:int -> V.t -> V.t -> t -> t
  val dfs : V.t -> t -> VSet.t
  val connected_components : t -> VSet.t list
  val kruskal : ?steps:int -> t -> (V.t * V.t * int) list
end
