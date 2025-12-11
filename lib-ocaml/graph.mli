module type Point = sig
  type t

  val compare : t -> t -> int
  val pr : t -> string
end

module MakeUF (V : Point) : sig
  type t

  val empty : unit -> t

  val find : V.t -> t -> V.t
  val unite : V.t -> V.t -> t -> t
  val connected : V.t -> V.t -> t -> bool
end

module Make (V : Point) : sig
  type t

  module VSet : sig include Set.S with type elt = V.t end

  val empty : unit -> t

  val add_directed_edge : ?cost:int -> V.t -> V.t -> t -> t
  val add_undirected_edge : ?cost:int -> V.t -> V.t -> t -> t
  val dfs : V.t -> t -> VSet.t
  val topo_sort : t -> V.t list
  val connected_components : t -> VSet.t list
  val kruskal : ?steps:int -> t -> (V.t * V.t * int) list
  val count_paths_with_cstrs : V.t list -> V.t -> V.t -> t -> int
  val count_paths : V.t -> V.t -> t -> int
end
