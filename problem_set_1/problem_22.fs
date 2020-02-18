(*
  Write an uncurried F# function cartesian (xs, ys) that takes as input two
  lists xs and ys and returns a list of pairs that represents the Cartesian
  product of xs and ys. (The pairs in the Cartesian product may appear in any
  order.)
*)

let rec cartesian_rec acc = function
| (_, []) | ([], _) -> ([], [])
| x::xs, ys -> List.map (fun y -> (x, y)) ys :: acc :: cartesian_rec acc (xs, ys)

let cartesian xs ys = cartesian_rec (xs, ys)

[<EntryPoint>]
let main _ =
  printfn "%A" cartesian [ "a"; "b"; "c" ] [ 1; 2 ]
