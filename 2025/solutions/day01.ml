open Lib.Int

type rotation = Left of int | Right of int

let rec parse_input (rotas : string list) : rotation list =
  match rotas with
  | [] -> []
  | x :: xs ->
     match String.to_seq x () with
     | Nil -> failwith "Illegal input: empty string"
     | Cons (c, rota) ->
        (match c with
        | 'L' -> Left (int_of_string (String.of_seq rota))
        | 'R' -> Right (int_of_string (String.of_seq rota))
        | _ -> failwith (Printf.sprintf "Illegal input: got %c while expecting L or R" c)) ::
          parse_input xs

let solve (rotations : rotation list) (dial : int)
      (on_left : int -> int -> int * int) (on_right : int -> int -> int * int) : int =
  let rec aux rotations dial =
    match rotations with
    | [] -> 0
    | x :: xs ->
       let (dial, n) =
         match x with
         | Left k -> on_left dial k
         | Right k -> on_right dial k in
       n + aux xs dial in
  aux rotations dial

let solve1 (rotations : rotation list) (dial : int) : int =
  let mk_solve1_pair dial =
    (dial, if dial = 0 then 1 else 0) in
  solve rotations dial
    (fun dial k -> mk_solve1_pair ((dial - k) mod 100))
    (fun dial k -> mk_solve1_pair ((dial + k) mod 100))

let part1 (input : string list) : string =
  let rotations = parse_input input in
  string_of_int (solve1 rotations 50)

let solve2 (rotations : rotation list) (dial : int) : int =
  let mk_solve2_pair cond dial k =
    (dial, (k / 100) + if cond dial then 1 else 0) in
  solve rotations dial
    (fun dial k -> mk_solve2_pair
                  (* Here, we have to offset the dial by 100 to avoid counting e.g.,
                     when the dial was 0 and we turn it once to the left.
                     But then, it also means that we have to explicitely count when
                     the new dial value is 0. *)
                  (fun d -> d = 0 || d > (if dial = 0 then 100 else dial))
                  ((dial - k) mod 100) k)
    (fun dial k -> mk_solve2_pair (fun d -> d < dial) ((dial + k) mod 100) k)

let part2 (input : string list) : string =
  let rotations = parse_input input in
  string_of_int (solve2 rotations 50)
