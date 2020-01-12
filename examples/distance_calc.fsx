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
let distance (p1: Point) (p2: Point) : float =
  let x_f = pown (p2.x - p1.x) 2
  let y_f = pown (p2.y - p1.y) 2
  sqrt (x_f + y_f)

let main () =
  // Convert commandline string arguements into an Array and remove
  // the first element (the command executing).
  let args: string array = fsi.CommandLineArgs |> Array.tail

  if args.Length < 4 then
    eprintfn "%s" "Number of input paramenters is too short"
  else
    // Define Point #1
    let point1: Point =
      { x = (args.[0] |> float);
        y = (args.[1] |> float) }
    // Define Point #2
    let point2: Point =
      { x = (args.[2] |> float);
        y = (args.[3] |> float) }

    printfn "Finding the distance between Point (%f, %f) and Point (%f, %f)" point1.x point1.y point2.x point2.y

    let result = distance point1 point2

    printfn "The distance between Point (%f, %f) and Point (%f, %f) is %f" point1.x point1.y point2.x point2.y result

main()
