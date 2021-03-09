let rec len_ acc l = match l with
  | [] -> acc
  | _::xs -> len_ (acc + 1) xs

let len l =
  len_ 0 l

let main () =
  if len [ "a" ; "b" ; "c" ; "d" ] = 4 &&
     len [] = 0 then
       Io.format "ok\n" []
  else
    Io.format "!!\n" []
