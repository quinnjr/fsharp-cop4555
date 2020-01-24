// Print greeting example
//
// Program asks user for input on the command line and awaits input.
// Upon input, Program reads the line(s) and prints out only names
// 'Bob' and 'Alice'

open System

let rec rprint arr =
  match arr with
  | p :: tail when p = "Bob" || p = "Alice" ->
    printfn "Hello %s!" p
    rprint tail
  | _ :: tail -> rprint tail
  | [] -> ()

[<EntryPoint>]
let main args =
  let input = []
  let line = Console.ReadLine()

  while line <> null do
    printfn "What is your name?: "
    let input = input @ [line] @ []

  rprint input

  0
