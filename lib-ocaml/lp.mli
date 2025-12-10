
(** Small API to use lp_solve *)

type lp_object = Int of int | Float of float | Var of string

val fresh_var : unit -> lp_object

type lp_formula = Ob of lp_object | Add of lp_formula * lp_formula
                  | Mul of lp_formula * lp_formula

(** Makes a sum (fold of Add) of the formula list *)
val sum : lp_formula list -> lp_formula

type lp_objective_optim = Min | Max
type lp_objective_func  = lp_objective_optim * lp_formula

type lp_kind = Lt | Le | Eq | Gt | Ge
type lp_constraint = IsInt of lp_object | Bin of lp_formula * lp_kind * lp_formula

type lp_problem =
  { objective_func: lp_objective_func
  ; constraints: lp_constraint list }

val pr_lp_problem : lp_problem -> string

(** Solves the lp problem and returns the value of the objective function as an integer.
    Always add the [IsInt] constraints after the normal constraints (otherwise it will have
    a parsing error) *)
val solve_objective_ilp : lp_problem -> int
