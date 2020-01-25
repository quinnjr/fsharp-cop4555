// Print greeting example
//
// Program asks user for input on the command line and awaits input.
// Upon input, Program reads the line(s) and prints out only names
// 'Bob' and 'Alice'

open System

[<EntryPoint>]
let main args =
  let rec rprintfn l =
    match l with
    | [] -> printf ""
    | head :: tail when head = "Alice" || head = "Bob"->
      printfn "Hello %s!" head
      rprintfn tail
    | _ :: tail -> rprintfn tail

  let args = args |> List.ofArray

  rprintfn args

  0
