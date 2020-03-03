(*
Write an uncurried F# function to do matrix multiplication.
Assume that the dimensions of the matrices are appropriate.
*)

let rec transpose = function
| [] | []::_ -> []
| (x::xs')::ys as M -> List.map List.head M :: transpose(List.map List.tail M)

let rec inner xs ys =
  if List.length xs <> List.length ys then
    failwith "Exception: Lists are of unequal length"
  else
    let rec sigma acc = function
    | ([], []) | (_, []) | ([], _) -> acc
    | (x::xs, y::ys) -> sigma ((x * y) + acc) (xs, ys)
    sigma 0 (xs, ys)

let multiply xs ys = inner xs (transpose ys)

[<EntryPoint>]
let main _ =
  printfn "%A" multiply ([[1;2;3];[4;5;6]], [[0;1];[3;2];[1;2]])
  0
