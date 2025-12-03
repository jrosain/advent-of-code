
let (mod) n m : int =
  ((n mod m) + m) mod m

let rec pow x n =
  match n with
  | 0 -> 1
  | 1 -> x
  | n ->
     let y = pow x (n / 2) in
     y * y * (if n mod 2 = 0 then 1 else x)

let log10 n : int =
  Float.to_int @@ log10 (float_of_int n)

let digits n : int =
  log10 n + 1

let nth_digit ~(digit : int) (n : int) : int =
  (n / (pow 10 digit)) mod 10
