(** This file implements the function register, which manages the function to call for a
 ** specific day. *)

type t

type solution_func = (string list -> string) * (string list -> string)

val add : day:int -> solution_func -> unit

val get : day:int -> solution_func option
