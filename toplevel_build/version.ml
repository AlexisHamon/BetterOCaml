let from_string v = List.map int_of_string (String.split_on_char '.' v)
  
let rec comp v v' = match v, v' with
  | [], [] -> 0
  | [], y::ys -> if y = 0 then comp [] ys else -1
  | x::xs, [] -> if x = 0 then comp xs [] else 1
  | x::xs, y::ys -> if x = y then comp xs ys else compare x y

let current = from_string Sys.ocaml_version