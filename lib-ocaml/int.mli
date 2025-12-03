
(** (infix) modulo operation for which (-1 mod x) (x positive) returns (x - 1). *)
val (mod) : int -> int -> int

(** Fast integer exponentiation *)
val pow   : int -> int -> int

(** Truncated log10 operation on integers. *)
val log10 : int -> int

(** Number of digits of a number *)
val digits : int -> int

(** Get the nth digit of a base-10 integer *)
val nth_digit : digit:int -> int -> int
