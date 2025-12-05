open Lib

let parse : string list -> Interval.t list * int list =
  let fold acc s =
    if String.contains s '-'
    then
      match String.split_on_char '-' s with
      | [a ; b] -> (Interval.make (int_of_string a) (int_of_string b) :: fst acc,
                  snd acc)
      | _ -> failwith (Printf.sprintf "Illegal input: interval %s has more than two bounds" s)
    else if not @@ String.equal "" s
    then (fst acc, int_of_string s :: snd acc)
    else acc in
  List.fold_left fold ([], [])

let get_related_intervals (x : int) : Interval.t list -> Interval.t list =
  List.filter (Interval.mem x)

let part1 (input : string list) : string =
  let intervals, vals = parse input in
  let vals = List.filter (fun x -> not @@ List.is_empty (get_related_intervals x intervals)) vals in
  string_of_int (List.length vals)

let merge_all_intervals (intervals : Interval.t list) : Interval.t list =
  let rec aux (intervals : Interval.t list) : Interval.t list =
    match intervals with
    | [] -> []
    | i :: is ->
       let is' = List.filter (Interval.overlap i) is in
       if List.is_empty is' then i :: aux is
       else
         let i0 = List.fold_left Interval.merge i is' in
         let is'' = List.filter (fun i -> not @@ List.mem i is') is in
         i0 :: aux is'' in
  let rec iterate (intervals : Interval.t list) : Interval.t list =
    let new_intervals = aux intervals in
    if intervals = new_intervals then intervals
    else iterate new_intervals in
  iterate intervals

let part2 (input : string list) : string =
  let intervals, _ = parse input in
  let intervals = merge_all_intervals intervals in
  string_of_int @@ List.fold_left (+) 0 @@ List.map Interval.len intervals

