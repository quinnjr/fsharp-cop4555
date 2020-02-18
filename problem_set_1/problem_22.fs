(*
  Write an uncurried F# function cartesian (xs, ys) that takes as input two
  lists xs and ys and returns a list of pairs that represents the Cartesian
  product of xs and ys. (The pairs in the Cartesian product may appear in any
  order.)
*)

let rec acc_cartesian acc = function
| ([], []) -> acc
| (x::xs, y::ys) -> acc_cartesian (x,y)::acc xs ys

let rec cartesian xs ys = acc_cartensian [] (xs, ys)

[<EntryPoint>]
let main _ =
  printfn "%A" cartesian [ "a"; "b"; "c" ] [ 1; 2 ]
