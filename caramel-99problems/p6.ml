(* # is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
 * - : bool = true
 * # not (is_palindrome [ "a" ; "b" ]);;
 * - : bool = true *)

let rec is_palindrome_ l1 l2 = match (l1, l2) with
  | ([], []) -> true
  | ([x], [y]) when x = y -> true
  | (x::xs, y::ys) when x = y -> is_palindrome_ xs ys
  | _ -> false

let is_palindrome l = is_palindrome_ l (Lists.reverse l)

let main () =
  if is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ] = true
  && is_palindrome [ "a" ; "b" ] = false then
    Io.format "ok\n" []
  else
    Io.format "!!\n" []
