(** Top-level of the AOC cli: automate reading of input files w.r.t the current day *)
let main () =
  let time = Unix.localtime (Unix.time ()) in
  let day = ref time.Unix.tm_mday in
  let infile = ref "" in
  let from_stdin = ref false in
  let optlist = [
      ("-day", Arg.Set_int day, "Use this day instead of the current month's day");
      ("-input", Arg.Set_string infile, "Use specified input file instead of the one named day.txt in the input folder");
      ("-stdin", Arg.Set from_stdin, "Read from stdin instead of the input file");
    ] in
  let _ = Arg.parse optlist (fun _ -> ()) "" in
  let infile =
    match !infile with
    | "" -> Printf.sprintf "inputs/%02d.txt" !day
    | _  -> !infile in
  let input =
    try
      if !from_stdin then stdin
      else open_in infile
    with
    | Sys_error _ -> failwith (Printf.sprintf "File %s not found" infile) in
  Solutions.Init.register_all_days();
  match Lib.FuncRegister.get ~day:!day with
  | None -> failwith (Printf.sprintf "No registered solution for the day %d" !day)
  | Some (f1, f2) ->
     let time f x =
       let t = Sys.time() in
       let r = f x in
       (Sys.time() -. t, r) in
     let lines = In_channel.input_lines input in
     let time1, part1 = time f1 lines in
     let time2, part2 = time f2 lines in
     Format.printf "Part 1 answer: %s [%fs]\nPart 2 answer: %s [%fs]"
       part1 time1 part2 time2

let _ = main ()
