
module Mask = struct
  type t = int

  let empty : t = 0

  let toggle (k : int) (mask : t) : t =
    Int.logxor (Lib.Int.pow 2 k) mask

  let is_toggled (k : int) (mask : t) : bool =
    Int.logand (Lib.Int.pow 2 k) mask = Lib.Int.pow 2 k

  let pr (mask : t) : string =
    let rec aux (i : int) : string =
      if i > 12 then ""
      else if is_toggled i mask
      then "#" ^ aux (i + 1)
      else "." ^ aux (i + 1) in
    aux 0
end

type machine = { diagram: Mask.t; wiring: (int list) list; joltage: int list }

(** Note: I replaced every space between buttons wiring by ; in the input *)
let remove_first_last (s : string) : string =
  (String.sub s 1 (String.length s - 2))

let parse_diagram (d : string) : Mask.t =
  let rec aux (l : string list) (i : int) (m : Mask.t) : Mask.t =
    match l with
    | [] -> m
    | x :: xs ->
       if x = "." then aux xs (i + 1) m
       else aux xs (i + 1) (Mask.toggle i m) in
  aux (Lib.Str.explode @@ remove_first_last d) 0 Mask.empty

let parse_wiring (w : string) : (int list) list =
  let rec aux (l : string list) : (int list) list =
    match l with
    | [] -> []
    | x :: xs ->
       let split_str = String.split_on_char ',' (remove_first_last x) in
       (List.map int_of_string split_str) :: aux xs in
  aux (String.split_on_char ';' w)

let parse_joltage (j : string) : int list =
  let split_str = String.split_on_char ',' (remove_first_last j) in
  List.map int_of_string split_str

let parse_line (s : string) : machine =
  let ls = String.split_on_char ' ' s in
  match ls with
  | [ d ; w ; j ] -> { diagram=parse_diagram d; wiring=parse_wiring w; joltage=parse_joltage j }
  | _ -> failwith ("Illegal input: got more than 3 elements." ^
                    "Have you replaced the spaces between buttons wiring by semicolons?")

let parse : string list -> machine list =
  let fold acc s = parse_line s :: acc in
  List.fold_left fold []

let solve1_ (m : machine) : int =
  let dp = Hashtbl.create 100000 in
  let rec get_next_mask (mask : Mask.t) (to_toggle : int list) : Mask.t =
    match to_toggle with
    | [] -> mask
    | k :: ks -> get_next_mask (Mask.toggle k mask) ks in

  let get_toggled_cost (mask : Mask.t) (current_cost : int) : (Mask.t * int) list =
    let rec aux (wirings : (int list) list) : (Mask.t * int) list =
      match wirings with
      | [] -> []
      | w :: wirings ->
         let mask = get_next_mask mask w in
         let cost = current_cost + 1 in
         match Hashtbl.find_opt dp mask with
         | None ->
            Hashtbl.add dp mask cost;
            (mask, cost) :: aux wirings
         | Some n ->
            if cost < n
            then
              (Hashtbl.replace dp mask cost;
               (mask, cost) :: aux wirings)
            else aux wirings in
    aux m.wiring in

  let rec step (next : (Mask.t * int) list) : (Mask.t * int) list =
    match next with
    | [] -> []
    | (m,c) :: mcs ->
       let next_candidates = get_toggled_cost m c in
       List.append next_candidates (step mcs) in
  let rec aux (next : (Mask.t * int) list) : int option =
    match next with
    | [] -> Hashtbl.find_opt dp m.diagram
    | _  -> aux (step next) in
  match aux [(Mask.empty, 0)] with
  | None -> failwith "Impossible"
  | Some n -> n

let solve1 (ms : machine list) : int =
  List.fold_left (+) 0 @@ List.map solve1_ ms

let part1 (input : string list) : string =
  let machines = parse input in
  string_of_int @@ solve1 machines

(** For part 2: we use linear programming. *)
let create_problem (m : machine) : Lib.Lp.lp_problem =
  let vars = List.init (List.length m.wiring) (fun _ -> Lib.Lp.fresh_var()) in
  let objective = (Lib.Lp.Min, Lib.Lp.sum (List.map (fun v -> Lib.Lp.Ob v) vars)) in
  let line j =
    let target = List.nth m.joltage j in
    let cs = List.filter_map (fun l -> if List.mem j l then List.find_index (fun l' -> l = l') m.wiring else None) m.wiring in
    let lhs = Lib.Lp.sum @@ List.map (fun k -> Lib.Lp.Ob (List.nth vars k)) cs in
    Lib.Lp.Bin (lhs, Lib.Lp.Eq, Lib.Lp.Ob (Lib.Lp.Int target)) in
  let cstrs = List.map line (List.init (List.length m.joltage) (fun n -> n)) in
  let cstrs = List.append cstrs @@ List.map (fun v -> Lib.Lp.IsInt v) vars in
  { objective_func=objective; constraints=cstrs }

let solve2_ (m : machine) : int =
  Lib.Lp.solve_objective_ilp @@ create_problem m

let solve2 (ms : machine list) : int =
  List.fold_left (+) 0 @@ List.map solve2_ ms

let part2 (input : string list) : string =
  let machines = parse input in
  string_of_int @@ solve2 machines
