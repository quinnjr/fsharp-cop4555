(*
  An m-by-n matrix can be represented in F# as a list of m rows,
  each of which is a list of length n. For example, the first matrix
  above is represented as the list [[1;2;3];[4;5;6]].

  Write an efficient F# function to compute the transpose of an m-by-n matrix.
*)

(*
  We work recursively on a matrix list M, looking at the matrix as its
  first list (x::xs) and the tail of the matrix ys.

  If the matrix is empty of the head of the matrix is empty, return an empty
  list.

  If we still have a matrix to transpose, use List.head as the transformation
  function on M to get the xs element by itself. Append the transposistion of
  the tail of M (ys) which has been transormed with List.map.

  Finally, return the new transposed list of lists.
*)

let rec transpose = function
| [] | []::_ -> []
| (x::xs')::ys as M -> List.map List.head M :: transpose(List.map List.tail M)

[<EntryPoint>]
let main _ =
  printfn "%A" (transpose [[1;2;3];[4;5;6]])

  0
