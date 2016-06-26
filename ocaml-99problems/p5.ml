let rev l =
  let rec rev' acc = function
  | [] -> acc
  | x::xs -> rev' (x::acc) xs
  in rev' [] l


let main =
  if rev [ "a" ; "b" ; "c" ; "d" ] = [ "d" ; "c" ; "b" ; "a" ] &&
     rev [ "a" ] = [ "a" ] &&
     rev [] = [] then
       print_endline "ok"
  else
    print_endline "!!"
