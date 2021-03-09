let length =
  let rec length' acc = function
    | [] -> acc
    | _::xs -> length' (acc + 1) xs in
  length' 0

let main =
  if length [ "a" ; "b" ; "c" ; "d" ] = 4 &&
     length [] = 0 then
       print_endline "ok"
  else
    print_endline "!!"
