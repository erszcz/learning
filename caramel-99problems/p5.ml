let rec rev_ acc l = match l with
  | [] -> acc
  | x::xs -> rev_ (x::acc) xs

let rev l = rev_ [] l

let main () =
  if rev [ "a" ; "b" ; "c" ; "d" ] = [ "d" ; "c" ; "b" ; "a" ] &&
     rev [ "a" ] = [ "a" ] &&
     rev [] = [] then
       Io.format "ok\n" []
  else
    Io.format "!!\n" []
