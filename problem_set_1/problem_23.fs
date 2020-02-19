(*
  An F# list can be thought of as representing a set, where the order of the
  elements in the list is irrelevant. Write an F# function powerset such that
  powerset set returns the set of all subsets of set. For example,
*)

let rec powerset = function
| [] -> [[]]
| x::xs -> List.collect (fun xs' -> [xs', x::xs']) powerset xs

let main _ =
  printfn "%A" powerset [1 .. 3]

  0
