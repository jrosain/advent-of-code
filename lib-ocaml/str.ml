
let explode (s : string) : string list =
  List.map (String.make 1) @@ List.of_seq @@ String.to_seq s

