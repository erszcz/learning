let rec at n l =
  let at' = 
    match (n, l) with
    | (1, x::_) -> Some x
    | (n', []) -> None
    | (n', _::xs) -> at (n-1) xs in
  if n < 1 then None
  else at'

let main =
  if at 3 [ "a" ; "b"; "c"; "d"; "e" ] = Some "c" &&
     at 3 ["a"] = None &&
     at 0 ["z"] = None then
       print_endline "ok"
  else
    print_endline "!!"
