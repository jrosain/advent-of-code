open Lib.Int

type range = { lo : int; hi : int }

let parse_input (input : string) : range list =
  let range_of_string s =
    let l = String.split_on_char '-' s in
    match l with
    | [lo_ ; hi_] -> { lo=int_of_string lo_ ; hi=int_of_string hi_ }
    | _ -> failwith (Printf.sprintf "Illegal input: malformed string %s" s) in
  List.map range_of_string (String.split_on_char ',' input)

let get_invalid_ids (is_invalid : int -> bool) (r : range) : int list =
  let rec aux n acc =
    match n with
    | n when n > r.hi -> acc
    | n when is_invalid n -> aux (n + 1) (n :: acc)
    | _ -> aux (n + 1) acc in
  aux r.lo []

let solve (input : string) (is_invalid : int -> bool) : int =
  let ranges = parse_input input in
  let invalid_ids = List.concat_map (get_invalid_ids is_invalid) ranges in
  List.fold_left (+) 0 invalid_ids

let is_id_invalid1 (id : int) : bool =
  let ndigits = digits id in
  let k = pow 10 (ndigits / 2) in
  id mod k = id / k

let part1 (input : string list) : string =
  string_of_int @@ solve (List.hd input) is_id_invalid1

let is_id_invalid2 (id : int) : bool =
  let rec repeat b k =
    if k = 0 then "" else b ^ (repeat b (k - 1)) in
  let rec aux k =
    if k = 0 then false
    else
      let b = string_of_int @@ id / (pow 10 k) in
      let s = repeat b (digits id / String.length b) in
      id = int_of_string s || aux (k - 1) in
  aux (digits id - 1)

let part2 (input : string list) : string =
  string_of_int @@ solve (List.hd input) is_id_invalid2
