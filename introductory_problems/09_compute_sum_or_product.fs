// Compute the sum or the product of 1 to n values
// with n being a number supplied by user input

open System

let rec product = function
| 1 -> 1
| i -> i * product (i - 1)

let rec sum = function
| 0 -> 0
| 1 -> 1
| i -> i + sum (i - 1)

[<EntryPoint>]
let main args =
  printfn "Input a number: "
  match System.Console.ReadLine() |> System.Int32.TryParse with
  | (true, i) ->
    printfn "What operation would you like to perform? (sum/product)"
    let operation = System.Console.ReadLine()
    let result =
      match operation with
      | o when o = "product" -> product i
      | o when o = "sum" -> sum i
      | _ -> eprintfn "Operation specified is not valid."; -1
    printfn "The result of running %s on %d is: %d" operation i result
    0
  | (false, _) -> eprintfn "Non-parseable value supplied"; -1
