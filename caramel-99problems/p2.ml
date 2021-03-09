let rec last_two l =
  match l with
  | [] | [_] -> None
  | x1 :: x2 :: [] -> Some (x1, x2)
  | _ :: xs -> last_two xs

let main =
  if last_two [ "a" ; "b" ; "c" ; "d" ] = Some ("c", "d") &&
     last_two [] = None then
       print_endline "ok"
  else
    print_endline "!!"
