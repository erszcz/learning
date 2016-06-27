(*
# (* There is no nested list type in OCaml, so we need to define one
     first. A node of a nested list is either an element, or a list of
     nodes. *)
  type 'a node =
    | One of 'a 
    | Many of 'a node list;;
type 'a node = One of 'a | Many of 'a node list

# flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
- : string list = ["a"; "b"; "c"; "d"; "e"]
*)

type 'a node =
    | One of 'a 
    | Many of 'a node list

let flatten deeplist =
    let rec flatten' acc = function
        | [] -> acc
        | One e :: t -> flatten' (e :: acc) t
        | Many es :: t -> flatten' ((flatten' [] es) @ acc) t
    in List.rev (flatten' [] deeplist)

let main =
    if flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ] = ["a"; "b"; "c"; "d"; "e"]
    then
        print_endline "ok"
    else
        failwith "!!"
