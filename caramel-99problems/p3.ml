let rec at n l = match l with
  | [] -> None
  | h :: t -> if n = 1 then Some h else at (n-1) t

let main () =
  if at 3 [ "a" ; "b"; "c"; "d"; "e" ] = Some "c" &&
     at 3 ["a"] = None &&
     at 0 ["z"] = None then
       Io.format "ok\n" []
  else
    Io.format "!!\n" []
