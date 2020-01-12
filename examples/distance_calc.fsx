// A simple calculator for the distance between two points on a
// 2D plane.
// Input from the commandline comes in the form of
// `x1` `y1` `x2` `y2`, where x's are the x-coordinates of the two
// two points and y's are the y coordinates of the two points.

// A simple 2D point
type Point = { x: float; y: float }

// Calculate the distance between Point 1 and Point 2.
// Note: Powers in F# on floats use the pown function.
// - ** is for floating point values, not integers.
// - ^ is not usable.
let distance (p: Point list) : float =
  let x_f = pown (p.[1].x - p.[0].x) 2
  let y_f = pown (p.[1].y - p.[0].y) 2
  sqrt (x_f + y_f)

let main () =
  // Convert commandline string arguements into an Array and remove
  // the first element (the command executing).
  let args: string array = fsi.CommandLineArgs |> Array.tail

  match args.Length < 4 with
  | true -> eprintfn "%s" "Number of input paramenters is too short"
  | false ->
    // Define our points
    let points = [
      { x = (args.[0] |> float); y = (args.[1] |> float) };
      { x = (args.[2] |> float); y = (args.[3] |> float) }
    ]

    printfn "Finding the distance between Point (%f, %f) and Point (%f, %f)" points.[0].x points.[0].y points.[1].x points.[1].y

    let result = distance points

    printfn "The distance between Point (%f, %f) and Point (%f, %f) is %f" points.[0].x points.[0].y points.[1].x points.[1].y result

main()
