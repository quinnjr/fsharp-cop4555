//
//
//

open System

// Note: Powers in F# on floats use the pown function.
// - ** is for floating point values, not integers.
// - ^ is not usable.
let distance (x1: float) (y1: float) (x2: float) (y2: float) : float =
  let x_f = (x2 - x1)**2
  let y_f = (y2 - y1)**2

  sqrt (x_f + y_f)

[<EntryPoint>]
let main args =
  // TODO: Single `let` statement
  let x1 = (args.[1] |> float)
  let y1 = (args.[2] |> float)
  let x2 = (args.[3] |> float)
  let y2 = (args.[4] |> float)

  printfn "Finding the distance between Point (%f, %f) and Point (%f, %f)" x1 y1 x2 y2

  // let result = distance x1 y1 x2 y2

  // printfn "The distance between Point (%f, %f) and Point (%f, %f) is %f" x1 y1 x2 y2 result
