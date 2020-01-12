// A simple calculator for the distance between two points on a
// 2D plane.
// Input from the commandline comes in the form of
// `x1` `y1` `x2` `y2`, where x's are the x-coordinates of the two
// two points and y's are the y coordinates of the two points.

// Calculate the distance between Point 1 and Point 2.
// Note: Powers in F# on floats use the pown function.
// - ** is for floating point values, not integers.
// - ^ is not usable.
let distance (x1: float) (y1: float) (x2: float) (y2: float) : float =
  let x_f = pown (x2 - x1) 2
  let y_f = pown (y2 - y1) 2

  sqrt (x_f + y_f)

let main () =
  // Convert commandline string arguements into an Array and remove
  // the first element (the command executing).
  let args: string array = fsi.CommandLineArgs |> Array.tail

  if args.Length < 4 then
    eprintfn "%s" "Number of input paramenters is too short"
  else
    let x1, y1, x2, y2 = (args.[0] |> float), (args.[1] |> float), (args.[2] |> float), (args.[3] |> float)

    printfn "Finding the distance between Point (%f, %f) and Point (%f, %f)" x1 y1 x2 y2

    let result = distance x1 y1 x2 y2

    printfn "The distance between Point (%f, %f) and Point (%f, %f) is %f" x1 y1 x2 y2 result

main()
