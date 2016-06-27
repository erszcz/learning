(*
# compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string list = ["a"; "b"; "c"; "a"; "d"; "e"]
*)

let compress l =
    let rec compress' current acc = function
        | [] -> current :: acc
        | x::xs when x = current -> compress' current acc xs
        | x::xs -> compress' x (current :: acc) xs
    in match l with
        | [] -> []
        | [x] -> [x]
        | x::xs -> List.rev (compress' x [] xs)

let main =
    if compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = ["a"; "b"; "c"; "a"; "d"; "e"]
    then
        print_endline "ok"
    else
        failwith "!!"
