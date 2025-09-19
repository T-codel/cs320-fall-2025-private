
let rec rev_append (l : 'a list) (r : 'a list) : 'a list =
  match l with
  | [] -> r
  | x :: xs -> rev_append xs (x :: r)

let is_whitespace c = List.mem c [' '; '\n'; '\t'; '\r']

let split_on_whitespace (s : string) : string list =
  let rec go acc i j =
    if i + j >= String.length s
    then List.rev (String.sub s i j :: acc)
    else
      if is_whitespace (String.get s (i + j))
      then go (String.sub s i j :: acc) (i + j + 1) 0
      else go acc i (j + 1)
  in go [] 0 0

let sep_on_whitespace (s : string) : string list =
  let rec go acc l =
    match l with
    | [] -> List.rev acc
    | h :: t ->
      if h = ""
      then go acc t
      else go (h :: acc) t
  in go [] (split_on_whitespace s)

type registers = (int * int) list

let load (_l : int list) : registers = assert false

let lookup (_rs : registers) (_i : int) : int = assert false

let incr (_rs : registers) (_i : int) : registers = assert false

let zero (_rs : registers) (_i : int) : registers = assert false

let transfer (_rs : registers) (_i : int) (_j : int) : registers = assert false

let parse_urm (_tokens : string list) : int list list = assert false

let eval_urm (_prog : int list list) (_rs : registers) : registers = assert false

let interp_urm (prog : string) (args : int list) : int =
  prog
  |> sep_on_whitespace
  |> parse_urm
  |> fun prog -> eval_urm prog (load args)
  |> fun rs -> lookup rs 0

(* Challenge problem: make this work for negative inputs *)
let max_urm (i : int) (j : int) : int =
  interp_urm
    "
    T 0 2
    Z 0
    J 1 3 100
    I 0
    I 3
    J 0 0 2
    "
    [i; j]

let fibonacci_urm (i : int) : int =
  interp_urm
    "
    I 2
    J 0 5 11
      T 2 3
      J 1 4 7
        I 2 I 4
        J 0 0 3
      T 3 1
      Z 4 I 5
      J 0 0 1
    T 1 0
    "
    [i]
