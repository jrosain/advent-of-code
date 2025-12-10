
type segment = { st: Coord.t; ed: Coord.t }

let make_segment (c1 : Coord.t) (c2 : Coord.t) : segment =
  { st=c1; ed=c2 }

let pr_segment (s : segment) : string =
  Printf.sprintf "%s--%s" (Coord.pr s.st) (Coord.pr s.ed)

let is_on_segment (point : Coord.t) (segment : segment) : bool =
  let v1 = Coord.vec segment.st segment.ed in
  let v2 = Coord.vec segment.st point in
  (* The 3 points are not aligned *)
  if Coord.cross v1 v2 <> 0 then false
  else
    let k1 = Coord.dot v1 v2 in
    if k1 < 0 then false
    else if k1 = 0 then true
    else
      let k2 = Coord.dot v1 v1 in
      if k1 > k2 then false
      else true

let project_point_on_line (point : Coord.t) (line : segment) : Coord.t =
  let ap = Coord.vec point line.st in
  let ab = Coord.vec line.st line.ed in
  Coord.add line.st
    (Coord.mul (Coord.dot ap ab / Coord.dot ab ab) ab)

type orientation = Collinear | Clockwise | CounterClockwise

let get_orientation (p : Coord.t) (q : Coord.t) (r : Coord.t) : orientation =
  let v = (Coord.row q - Coord.row p) * (Coord.col r - Coord.col q) -
            (Coord.col q - Coord.col p) * (Coord.row r - Coord.row q) in
  match v with
  | 0 -> Collinear
  | x when x > 0 -> Clockwise
  | _ -> CounterClockwise

let segment_intersect (s1 : segment) (s2 : segment) : bool =
  let o1 = get_orientation s1.st s1.ed s2.st in
  let o2 = get_orientation s1.st s1.ed s2.ed in
  let o3 = get_orientation s2.st s2.ed s1.st in
  let o4 = get_orientation s2.st s2.ed s1.ed in
  o1 <> o2 && o3 <> o4

type polygon = segment list

let make_polygon (ls : Coord.t list) : polygon =
  let rec get_segments (ls : Coord.t list) : polygon =
    match ls with
    | c1 :: c2 :: cs -> { st=c1; ed=c2 } :: get_segments (c2 :: cs)
    | _ -> [] in
  get_segments (List.append ls [List.hd ls])

let is_inside_polygon (polygon : polygon) (point : Coord.t) : bool =
  let x = Coord.col point in
  let y = Coord.row point in
  let rec aux (acc : bool) (polygon : polygon) : bool =
    match polygon with
    | [] -> acc
    | line :: cs ->
       if is_on_segment point line then true
       else
         if (Coord.row line.st > y) <> (Coord.row line.ed > y) &&
              x < Coord.col (project_point_on_line point line)
         then aux (not acc) cs
         else aux acc cs in
  aux false polygon

type rectangle = { x1: Coord.t; x2: Coord.t; x3: Coord.t; x4: Coord.t; }

let make_rectangle (x : Coord.t) (y : Coord.t) : rectangle =
  { x1=Coord.make (min (Coord.col x) (Coord.col y)) (min (Coord.row x) (Coord.row y))
  ; x2=Coord.make (max (Coord.col x) (Coord.col y)) (min (Coord.row x) (Coord.row y))
  ; x3=Coord.make (min (Coord.col x) (Coord.col y)) (max (Coord.row x) (Coord.row y))
  ; x4=Coord.make (max (Coord.col x) (Coord.col y)) (max (Coord.row x) (Coord.row y)) }

let rectangle_segments (r : rectangle) : polygon =
  make_polygon [r.x1; r.x2; r.x4; r.x3]

let pr_rectangle (r : rectangle) : string =
  Printf.sprintf "Rectangle(%s, %s, %s, %s)"
    (Coord.pr r.x1) (Coord.pr r.x2) (Coord.pr r.x3) (Coord.pr r.x4)

let is_rectangle_inside_polygon (p : polygon) (r : rectangle) : bool =
  let r' = rectangle_segments r in
  List.for_all (is_inside_polygon p) [r.x1; r.x2; r.x3; r.x4] &&
    List.for_all (fun s1 -> (List.fold_left (+) 0 @@
                            List.map (fun s2 -> Bool.to_int (segment_intersect s1 s2)) p) <= 2) r'
