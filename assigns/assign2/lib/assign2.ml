
let drop_leading (_n : int) (_l : int list) : int list =
  assert false

let drop_trailing (_n : int) (_l : int list) : int list =
  assert false

let split_on_char (_c : char) (_s : string) : string list =
  assert false

let parse_fractran (_input : string) : Q.t list =
  assert false

let eval_fractran (_program : Q.t list) (_input : Z.t) : Z.t =
  assert false

let interp_fractran (input : string) : Z.t -> Z.t =
  eval_fractran (parse_fractran input)

let max_fractran (i : int) (j : int) : int =
  let program = "5/6 5/2 5/3" in
  let input = Z.((~$2 ** i) * (~$3 ** j)) in
  let output = interp_fractran program input in
  let div_by_five n =
    let rec go acc n =
      if n = Z.one
      then acc
      else go (acc + 1) Z.(n / ~$5)
    in go 0 n
  in div_by_five output

let fib_fractran (n : int) : int =
  let program = "17/65 133/34 17/19 23/17 2233/69 23/29 31/23 74/341 31/37 41/31 129/287 41/43 13/41 1/13 1/3" in
  let input = Z.(~$78 * (~$5 ** n)) in
  let output = interp_fractran program input in
  let div_by_two n =
    let rec go acc n =
      if n = Z.one
      then acc
      else go (acc + 1) Z.(n / ~$2)
    in go 0 n
  in div_by_two output
