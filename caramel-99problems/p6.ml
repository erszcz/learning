(* # is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
 * - : bool = true
 * # not (is_palindrome [ "a" ; "b" ]);;
 * - : bool = true *)

let is_palindrome l =
    let rec is_palindrome' = function
        | ([], []) -> true
        | ([x], [y]) when x = y -> true
        | (x::xs, y::ys) when x = y -> is_palindrome' (xs, ys)
        | _ -> false
    in is_palindrome' (l, List.rev l)

let main =
    if is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ] = true &&
       is_palindrome [ "a" ; "b" ] = false then
           print_endline "ok"
    else
        failwith "!!"
