let rec last l =
  match l with
  | []      -> None
  | x :: [] -> Some x
  | x :: xs -> last xs

let main () =
  if last [1; 2] = Some 2 then
    Io.format "ok\n" []
  else
    Io.format "!!\n" []
