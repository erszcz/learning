(*
# compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string list = ["a"; "b"; "c"; "a"; "d"; "e"]
*)

let rec compress_ current acc l = match l with
  | [] -> current :: acc
  | x::xs when x = current -> compress_ current acc xs
  | x::xs -> compress_ x (current :: acc) xs

let compress l = match l with
  | [] -> []
  | [x] -> [x]
  | x::xs -> Lists.reverse (compress_ x [] xs)

let main () =
  if compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = ["a"; "b"; "c"; "a"; "d"; "e"]
  then
    Io.format "ok\n" []
  else
    Io.format "!!\n" []
