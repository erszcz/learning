let rec last_two l =
  match l with
  | [] -> None
  | [_] -> None
  | x1 :: x2 :: [] -> Some (x1, x2)
  | _ :: xs -> last_two xs

let main () =
  if last_two [ "a" ; "b" ; "c" ; "d" ] = Some ("c", "d") &&
     last_two [] = None then
       Io.format "ok\n" []
  else
    Io.format "!!\n" []
