//
//

[<EntryPoint>]
let main args =
  printfn "Input a number: "
  // Wait for user input
  // Use input is a string, so it must be parsed to an int
  let input = match System.Console.ReadLine() |> Int32.TryParse with
  | true, i -> i
  | _ -> eprintfn "Unable to parse input into a valid integer"

  let rec zero_to_n in =
    let out = match in = 0 with
    | true -> 0
    | false -> zero_to_n in - 1

    out + 1

  printfn "Sum from 1 to %d is %d" input zero_to_n input
