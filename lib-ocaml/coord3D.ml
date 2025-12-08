
type t = { x: int; y: int; z: int }

let make x y z : t = { x; y; z }

let compare v1 v2 =
  if compare v1.x v2.x = 0
  then if compare v1.y v2.y = 0
       then compare v1.z v2.z
       else compare v1.y v2.y
  else compare v1.x v2.x

let dist v1 v2 =
  let x = v1.x - v2.x in
  let y = v1.y - v2.y in
  let z = v1.z - v2.z in
  x*x + y*y + z*z

let pr v = Printf.sprintf "(%d, %d, %d)" v.x v.y v.z

let x v = v.x
