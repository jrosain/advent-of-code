open Lib

module G = Graph.Make(Str)

let parse (input : string list) : G.t =
  let rec aux (g : G.t) (lines : string list) : G.t =
    match lines with
    | [] -> g
    | x :: xs ->
       let ls = String.split_on_char ' ' x in
       let v = String.sub (List.hd ls) 0 (String.length (List.hd ls) - 1) in
       let g = List.fold_left (fun g v' -> G.add_directed_edge v v' g) g (List.tl ls) in
       aux g xs in
  aux (G.empty()) input


let part1 (input : string list) : string =
  let g = parse input in
  string_of_int @@ G.count_paths "you" "out" g

let part2 (input : string list) : string =
  let g = parse input in
  string_of_int @@ G.count_paths_with_cstrs ["dac" ; "fft"] "svr" "out" g

