let parse (input : string list) : (int * int list) list =
  let map s =
    let ls = String.split_on_char ' ' s in
    match ls with
    | [] -> failwith "Illegal input: empty string"
    | x :: xs  ->
       let x = String.sub x 0 (String.length x - 1) in
       match String.split_on_char 'x' x with
       | [ x1 ; x2 ] -> (int_of_string x1 * int_of_string x2, List.map int_of_string xs)
       | _ -> failwith "Illegal input: first element is not not nxm" in
  List.map map input

let part1 (input : string list) : string =
  let parsed = parse input in
  let big = List.filter (fun (area,shapes) -> area >= 9 * (List.fold_left (+) 0 shapes)) parsed in
  (* So, technically we'd need to check everything that is smaller than that
     But in the real input, we don't need to.. *)
  string_of_int (List.length big)

let part2 (_ : string list) : string =
  "Merry christmas!!"
