
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

let load (l : int list) : registers = 
   let rec build_registers acc i lst =    
    match lst with
    | [] -> List.rev acc    
    | head :: rest ->       
        if head = 0 then
          build_registers acc (i + 1) rest   
        else
          build_registers ((i, head) :: acc) (i + 1) rest
  in
  build_registers [] 0 l





let lookup (rs : registers) (i : int) : int =
  let rec aux list =
    match list with
    | [] -> 0 (*if empty list return 0*)
    | (register, value) :: tail -> (*otherwise pattern match the tuple registers*)
        if register = i then value (*if the proper register is reached, return the associated value*)
        else aux tail  (*else, continue searching*)
  in
  aux rs (*calls the main function*)





let incr (rs : registers) (i : int) : registers =
  let rec update_registers updated remaining =
    match remaining with
    | [] -> List.rev updated @ [(i, 1)]
    | (register, value) :: tail -> (*iterate over the register values*)
        if register = i then (*if we arrived at an increment command*)
          List.rev updated @ ((register, value + 1) :: tail) (*increment*)
        else if register > i then
          List.rev updated @ [(i, 1)] @ (register, value) :: tail (*keeps track of default*)
        else
          update_registers ((register, value) :: updated) tail
    in
  update_registers [] rs




let zero (rs : registers) (i : int) : registers =
  let rec aux found rest =
    match rest with
    | [] -> List.rev found  (*returns the generated list.*)
    | (j, v) :: tail ->
        if j = i then
          List.rev found @ tail    (* skips the value *)
        else
          aux ((j, v) :: found) tail
  in
  aux [] rs

let exists (rs : registers) (i : int) : bool =
  let rec aux l =
    match l with
    | [] -> false (*if it has not been found earlier return false*)
    | (register, _) :: tail ->
        if register = i (*for every register, loop until the element is found and return true if it's found*)
          then true
       else aux tail
  in
  aux rs  

let transfer (rs : registers) (i : int) (j : int) : registers =
  if not (exists rs i) then []
  else
    let val_i = lookup rs i in
    if val_i = 0 then rs (*handles no transfer case*)
    else
      let rec aux found rest inserted =
        match rest with
        | [] ->
            if inserted then List.rev found
            else List.rev ((j, val_i) :: found)
        | (k, v) :: tail ->
            if k = j then aux ((k, val_i) :: found) tail true (*inserts the values of k, val_i *)
            else if k > j && not inserted then aux ((j, val_i) :: (k, v) :: found) tail true (*this code continues to search recursively*)
            else aux ((k, v) :: found) tail inserted (*adds the rest of the list*)
      in
      aux [] rs false





exception Invalid_argument
let parse_urm (tokens : string list) : int list list =
  let rec aux acc tokens =
    match tokens with
    | [] -> List.rev acc 
    | "Z" :: i :: rest -> (*handles zeroing case*)
        let i_int = int_of_string i in
        aux ([0; i_int] :: acc) rest
    | "I" :: i :: rest -> (*handles increment case*)
        let i_int = int_of_string i in
        aux ([1; i_int] :: acc) rest
    | "T" :: i :: j :: rest -> (*handles transfer*)
        let i_int = int_of_string i in
        let j_int = int_of_string j in
        aux ([2; i_int; j_int] :: acc) rest
    | "J" :: i :: j :: k :: rest -> (*parses jump command*)
        let i_int = int_of_string i in
        let j_int = int_of_string j in
        let k_int = int_of_string k in
        aux ([3; i_int; j_int; k_int] :: acc) rest
    | _ -> raise Invalid_argument
  in
  aux [] tokens




exception Invalid_Instruction
let eval_urm (prog : int list list) (rs : registers) : registers =
  let rec get_ith_instruction prog i =
    match prog with
    | [] -> None
    | instr :: rest ->
        if i = 0 then Some instr
        else get_ith_instruction rest (i - 1) (*handles iterating over instructions.*)
  in

  let rec next pc rs =
    match get_ith_instruction prog pc with
    | None -> rs  (* program counter out of bounds*)
    | Some instr ->
      match instr with
      | [0; i] -> next (pc + 1) (zero rs i) (*applies the proper function and updates pointer*)
      | [1; i] -> next (pc + 1) (incr rs i)
      | [2; i; j] -> next (pc + 1) (transfer rs i j)
      | [3; i; j; k] ->
          if lookup rs i = lookup rs j then next k rs
          else next (pc + 1) rs
      | _ -> raise Invalid_Instruction
  in

  next 0 rs

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
