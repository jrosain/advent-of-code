open Lib.Int

module IntSet = Set.Make(Int)

type range = { lo : int; hi : int }

let parse_input (input : string) : range list =
  let range_of_string s =
    let l = String.split_on_char '-' s in
    match l with
    | [lo_ ; hi_] -> { lo=int_of_string lo_ ; hi=int_of_string hi_ }
    | _ -> failwith (Printf.sprintf "Illegal input: malformed string %s" s) in
  List.map range_of_string (String.split_on_char ',' input)

(** We generate all the invalid ids in a range.

    To do so, remark that in general, a number d with a repeating pattern can be
    written d = d₀d₁(...)dₘd₀d₁(...)dₘ(...)d₀d₁(...)dₘ, i.e., there are n repetitions
    of the pattern d₀d₁(...)dₘ (that has m digits).

    In particular, we can rewrite this number as follows:
    d = k × 10ᵐ⁽ⁿ⁻¹⁾ + k × 10ᵐ⁽ⁿ⁻²⁾ + ... + k × 10ᵐ⁽ⁿ⁻ⁿ⁾
      = k × (10ᵐ⁽ⁿ⁻¹⁾ + 10ᵐ⁽ⁿ⁻²⁾ + 1).

    Let c = (10ᵐ⁽ⁿ⁻¹⁾ + 10ᵐ⁽ⁿ⁻²⁾ + 1). So in fact, what we want to do is to generate all
    the factors of c in the range [a, b].

    This is simply done by taking the following range: [a/c, b/c] × c.

    Then, we can solve today's problem:
    - part 1: we force n = 2. It suffices to generate all the invalid ids for m between
      (digits a / 2) and (digits b / 2).
    - part 2: here, n will be between 2 and digits b, so we generate all the invalid ids
      for m between (digits a / n) and (digits b / n) for every n. *)

(** Computes (10ᵐ⁽ⁿ⁻¹⁾ + 10ᵐ⁽ⁿ⁻²⁾ + 1). *)
let factor (m : int) (n : int) : int =
  let rec aux k =
    match k with
    | 0 -> 1
    | _ -> pow 10 (m * k) + aux (k - 1) in
  aux (n - 1)

(** Innermost invalid id generation loop, with:
    - m: number of digits of the pattern
    - n: number of repetitions of the pattern
    - a: start of the range
    - b: end of the range *)
let generate (m : int) (n : int) (a : int) (b : int) : IntSet.t =
  let c = factor m n in
  let rec aux k b =
    if k > b then IntSet.empty
    else if digits k = m then IntSet.add (k * c) (aux (k + 1) b)
    else aux (k + 1) b in
  let st = if a mod c = 0 then a / c else (a / c) + 1 in
  aux st (b / c)

let all_invalid_ids (n : int) (r : range) : IntSet.t =
  let rec invalid_ids_of_size (d : int) (ed : int) : IntSet.t =
    if d > ed
    then IntSet.empty
    else IntSet.union (generate d n r.lo r.hi) (invalid_ids_of_size (d + 1) ed) in
  invalid_ids_of_size ((digits r.lo) / n) ((digits r.hi) / n)

let sum (s : IntSet.t) : int = IntSet.fold (+) s 0

let part1 (input : string list) : string =
  let ranges = parse_input (List.hd input) in
  let invalid_ids =
    List.fold_left (fun s r -> IntSet.union (all_invalid_ids 2 r) s)
      IntSet.empty ranges in
  string_of_int @@ sum invalid_ids

let part2 (input : string list) : string =
  let ranges = parse_input (List.hd input) in
  let rec aux (r : range) (st : int) (ed : int) : IntSet.t =
    if st > ed then IntSet.empty
    else IntSet.union (all_invalid_ids st r) (aux r (st + 1) ed) in
  let fold (s : IntSet.t) (r : range) : IntSet.t =
    IntSet.union (aux r 2 (digits r.hi)) s in
  string_of_int @@ sum @@ List.fold_left fold IntSet.empty ranges

