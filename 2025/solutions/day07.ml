open Lib

let parse (input : string list) : Coord.t * Coord.Set.t =
  let splitters = Coord.from_grid '^' input in
  let start = Coord.from_grid 'S' input in
  let card = Coord.Set.cardinal start in
  if card <> 1
  then failwith (Printf.sprintf "Illegal input: found %d starts instead of one" card)
  else (Coord.Set.choose start, splitters)

let relevant_splitters (beams : Coord.Set.t) (splitters : Coord.Set.t) (lim : int) : Coord.Set.t =
  let fold beam (relevant_splitters, next_beams as acc) =
    if Coord.below beam lim
    then acc
    else
      let next = Coord.down beam in
      if Coord.Set.mem next splitters
      then
        (Coord.Set.add next relevant_splitters,
         Coord.Set.add (Coord.left next) @@
           Coord.Set.add (Coord.right next) next_beams)
      else (relevant_splitters, Coord.Set.add next next_beams) in
  let rec step (relevant_splitters : Coord.Set.t) (beams : Coord.Set.t) : Coord.Set.t =
    if Coord.Set.is_empty beams
    then relevant_splitters
    else
      let relevant_splitter, next =
        Coord.Set.fold fold beams (relevant_splitters, Coord.Set.empty) in
      step relevant_splitter next in
  step Coord.Set.empty beams

let part1 (input : string list) : string =
  let start, splitters = parse input in
  let h = List.length input in
  string_of_int @@
    Coord.Set.cardinal (relevant_splitters (Coord.Set.singleton start) splitters h)

let timelines (beams : int Coord.Map.t) (splitters : Coord.Set.t) (lim : int) : int Coord.Map.t =
  let upd c n m =
    Coord.Map.update c
      (fun k -> Some (match k with | None -> n | Some k -> k + n)) m in
  let fold beam n next_beams =
    if Coord.below beam lim
    then next_beams
    else
      let next_beams = upd beam (-n) next_beams in
      let next = Coord.down beam in
      if Coord.Set.mem next splitters
      then
        upd (Coord.left next) n @@
          upd (Coord.right next) n next_beams
      else upd next n next_beams in
  let rec step (beams : int Coord.Map.t) : int Coord.Map.t =
    let next_beams = Coord.Map.fold fold beams beams in
    if beams = next_beams then beams
    else step next_beams in
  step beams

let part2 (input : string list) : string =
  let start, splitters = parse input in
  let h = List.length input in
  string_of_int @@
    List.fold_left (+) 0 @@
      snd (List.split (Coord.Map.to_list (timelines (Coord.Map.singleton start 1) splitters h)))
