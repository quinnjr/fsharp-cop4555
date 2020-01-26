// A program which takes an input value and prints the sum from 1 to input

open System

// Recursive function to add only multiples of 3 and 5 are considered
let rec sum = function
| 0 -> 0
| x when (x % 3) = 0 || (x % 5) = 0 -> x + sum (x - 1)
| x -> sum (x - 1)

[<EntryPoint>]
let main args =
  printfn "Input a number: "
  // Wait for user input
  // Use input is a string, so it must be parsed to an Int32.
  let input =
    match System.Console.ReadLine() |> System.Int32.TryParse with
    | (true, i) -> Some(i)
    | (false, _) -> None

  // Match input as an Option<T> where Some(T)
  // is correct input and None prints an error.
  match input with
  | Some i ->
    let result = sum i
    printfn "Sum from 1 to %d is %d" input.Value result
    0
  | None ->
    eprintfn "Unable to parse input into a valid integer"
    -1
