//
//
//

open System

let distance (x1: int, y1: int) (x2: int, y2: int) : int =
  sqrt ((x2-x1)^2 + (y2-y1)^2)

[<EntryPoint>]
let main args =
  let x1, y1, x2, y2 = args.[1] |> int,
    args.[2] |> int, args.[3] |> int, args.[4] |> int
  let result = distance x1 y1 x2 y2

  printfn("The distance between (%i, %i) and (%i, %i) is %i"
  x1 y1 x2 y2 result)
