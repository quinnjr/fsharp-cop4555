// A program which takes an input value and prints the sum from 1 to input

open System

// Recursive function to sum values from 1 to N
let rec sum = function
| 0 -> 0
| 1 -> 1
// Recursively call sum with an argument of i - 1
// During spring-back from the base case, add the previous case to the
// current number
| i -> i + sum (i - 1)

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
