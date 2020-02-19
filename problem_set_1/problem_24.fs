(*
  An m-by-n matrix can be represented in F# as a list of m rows,
  each of which is a list of length n. For example, the first matrix
  above is represented as the list [[1;2;3];[4;5;6]].

  Write an efficient F# function to compute the transpose of an m-by-n matrix.
*)

let transpose matrix =
  let rec trp_rec = function
  | ([], []) | ([], _) | (_, []) -> [[];[]]
  | x::xs, y::ys -> x :: y :: (trp_rec (xs, ys))

  let m, n = matrix

  trp_rec (m, n)

[<EntryPoint>]
let main _ =
  printfn "%A" (transpose [[1 .. 3]; [4 .. 6]])
