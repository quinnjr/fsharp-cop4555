(*
Given vectors u = (u1, u2,..., un) and v = (v1, v2,..., vn),
the inner product of u and v is defined to be u1*v1 + u2*v2 + ... + un*vn.
Write a curried F# function inner that takes two vectors represented as
int list and returns their inner product.
*)

(* let rec sigma = function
| ([], []) | (_, []) | ([], _) -> 0I
| ([x], [y]) -> x * y
| (x::xs, y::ys) -> (x * y) + sigma (xs, ys) *)

let rec sigma acc = function
| ([], []) | (_, []) | ([], _) -> acc
| (x::xs, y::ys) -> sigma ((x * y) + acc) (xs, ys)

let rec inner xs ys =
  if List.length xs <> List.length ys then
    failwith "Exception: Lists are of unequal length"
  // else sigma (xs, ys)
  else sigma 0I (xs, ys)

[<EntryPoint>]
let main _ =
  printfn "%A" <| inner [1I..50000I] [50001I..100000I]

  0
