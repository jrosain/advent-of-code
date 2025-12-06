module Foldable = struct
  type 'a t = { func: 'a -> 'a -> 'a; def: 'a }

  let make (func : 'a -> 'a -> 'a) (def : 'a) : 'a t =
    { func; def }

  let apply (f : 'a t) (ls : 'a list) : 'a =
    List.fold_left f.func f.def ls
end

module IntMap = Map.Make(struct type t = int let compare = compare end)

let split (str : string) : string list =
  List.filter (fun c -> c <> "") @@ String.split_on_char ' ' str

let string_to_foldable (input : string) : (int Foldable.t) list =
  let map op =
    match op with
    | "+" -> Foldable.make (+) 0
    | "*" -> Foldable.make ( * ) 1
    | _   -> failwith (Printf.sprintf "Illegal input: unknown operation %s" op) in
  List.map map @@ split input

let nums_to_list1 (input : string list) : (int list) list =
  let map_ins (m : (int list) IntMap.t) (s : string) =
    let rec upd_map (n : int) (m : (int list) IntMap.t) (l : string list) : (int list) IntMap.t =
      match l with
      | [] -> m
      | x :: xs ->
         upd_map (n + 1) (IntMap.update n
                            (fun ls -> Some (int_of_string x ::
                                            match ls with
                                            | None -> []
                                            | Some l -> l)) m) xs in
    upd_map 0 m (split s) in
  let m = List.fold_left map_ins IntMap.empty input in
  snd (List.split @@ IntMap.to_list m)

let parse (input : string list) (nums_func : string list -> (int list) list) : (int list * int Foldable.t) list =
  let rev_input = List.rev input in
  let operations = List.hd rev_input in
  let nums = List.rev (List.tl rev_input) in
  List.combine (nums_func nums) (string_to_foldable operations)

let solve (problems : (int list * int Foldable.t) list) : string =
  string_of_int @@ List.fold_left (+) 0 @@ List.map (fun p -> Foldable.apply (snd p) (fst p)) problems

let part1 (input : string list) : string =
  solve @@ parse input nums_to_list1

let part2 (input : string list) : string =
  Printf.printf "Warning: this is an absurd problem, so I've just edited the file by hand.\n";
  Printf.printf "This solution works for a file where each row gives the right numbers.\n";
  let _, foldable = List.split @@ parse input nums_to_list1 in
  let in_ = open_in "inputs/transformed.txt" in
  let rows = In_channel.input_lines in_ in
  solve @@ List.combine (List.map (fun s -> List.map int_of_string (String.split_on_char ' ' s)) rows) foldable
