
type lp_object = Int of int | Float of float | Var of string

let pr_lp_object (ob : lp_object) : string =
  match ob with
  | Int n -> string_of_int n
  | Float f -> string_of_float f
  | Var v -> v

let vars = ref 0

let fresh_var () : lp_object =
  let obj = Var ("x" ^ string_of_int !vars) in
  vars := !vars + 1; obj

type lp_formula = Ob of lp_object | Add of lp_formula * lp_formula
                  | Mul of lp_formula * lp_formula

let rec pr_lp_formula (formula : lp_formula) : string =
  match formula with
  | Ob o -> pr_lp_object o
  | Add (f, g) -> pr_lp_formula f ^ " + " ^ pr_lp_formula g
  | Mul (f, g) -> pr_lp_formula f ^ " " ^ pr_lp_formula g

let rec sum (l : lp_formula list) : lp_formula =
  match l with
  | [] -> failwith "lp_sum: empty formula list"
  | [x] -> x
  | x1 :: x2 :: xs -> Add (x1, (sum @@ x2 :: xs))

type lp_objective_optim = Min | Max
type lp_objective_func  = lp_objective_optim * lp_formula

let pr_lp_objective_optim (optim : lp_objective_optim) : string =
  match optim with
  | Min -> "min"
  | Max -> "max"

let pr_lp_objective_func (objective_func : lp_objective_func) : string =
  pr_lp_objective_optim (fst objective_func) ^ ": " ^ pr_lp_formula (snd objective_func)

type lp_kind = Lt | Le | Eq | Gt | Ge
type lp_constraint = IsInt of lp_object | Bin of lp_formula * lp_kind * lp_formula

let pr_lp_kind (kind : lp_kind) : string =
  match kind with
  | Lt -> "<"
  | Le -> "<="
  | Eq -> "="
  | Gt -> ">"
  | Ge -> ">="

let pr_lp_constraint (cstr : lp_constraint) : string =
  match cstr with
  | IsInt ob -> "int " ^ pr_lp_object ob
  | Bin (f,k,g) -> pr_lp_formula f ^ " " ^ pr_lp_kind k ^ " " ^ pr_lp_formula g

type lp_problem =
  { objective_func: lp_objective_func
  ; constraints: lp_constraint list }

let pr_lp_problem (problem : lp_problem) : string =
  let objective = pr_lp_objective_func problem.objective_func in
  let cstrs = List.map pr_lp_constraint problem.constraints in
  List.fold_left (fun s s' -> s ^ ";\n" ^ s') objective cstrs ^ ";\n"

let solve_gen (problem : lp_problem) (args : string array) : string =
  let lp_solve = FileUtil.which "lp_solve" in
  let (temp_file, temp_channel) = Filename.open_temp_file "aoclplib" "" in
  at_exit (fun () -> Sys.remove temp_file);
  Printf.fprintf temp_channel "%s" @@ pr_lp_problem problem;
  flush temp_channel;
  let (read_fd, write_fd) = Unix.pipe () in
  match Unix.fork() with
  | 0 ->
     Unix.close read_fd;
     Unix.dup2 write_fd Unix.stdout;
     Unix.close write_fd;
     let _ = Unix.execv lp_solve @@ Array.append (Array.append [|lp_solve|] args) [|temp_file|] in
     exit 1
  | _ ->
     Unix.close write_fd;
     let buffer = Bytes.create 1024 in
     let _ = Unix.wait () in
     let bytes_read = Unix.read read_fd buffer 0 (Bytes.length buffer) in
     let output = Bytes.sub_string buffer 0 bytes_read in
     Unix.close read_fd; output

let solve_objective_ilp (problem : lp_problem) : int =
  let output = solve_gen problem [|"-S1"|] in
  let res = List.hd (List.rev @@ String.split_on_char ' ' output) in
  let res = List.hd (String.split_on_char '.' res) in
  int_of_string res
