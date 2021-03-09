exception Ex of string

let rec last l =
  match l with
  | []      -> None
  | x :: [] -> Some x
  | x :: xs -> last xs

let main =
  if last [1; 2] = Some 2 then
    print_endline "ok"
  else
    print_endline "!!"

(*

Notes:

- `=` and `==` are not equivalent!
  `=` is structural equality,
  `==` is physical equality (e.g. references pointing at the same object)

- it would be nice to run the tests with OUnit

*)
