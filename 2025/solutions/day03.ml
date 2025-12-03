open Lib.Int

type bank = int list

let parse_input (input : string list) : bank list =
  List.map (fun s -> List.map int_of_string @@ Lib.Str.explode s) input

let candidate_list (x : int) (k : int) : int list =
  let d = digits k in
  let rec aux (n : int) =
    if n = d
    then []
    else
      let p = d - n - 1 in
      let k0 = k mod (pow 10 p) in
      let k1 = k - (k mod (pow 10 (p + 1))) in
      k1 + 10 * k0 + x :: aux (n + 1) in
  aux 0

let turn_on (n : int) : bank -> int =
  let rec dp (k : int) (b : bank) : int =
    match b with
    | [] -> k
    | x :: xs ->
       if digits k < n
       then dp (10 * k + x) xs
       else
         let candidates = k :: candidate_list x k in
         dp (List.fold_left max 0 candidates) xs in
  dp 0

let solve (n : int) (input : string list) : string =
  let banks = parse_input input in
  string_of_int @@ List.fold_left (+) 0 @@ List.map (turn_on n) banks

let part1 : string list -> string = solve 2

let part2 : string list -> string = solve 12
