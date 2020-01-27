// Write a program that prints a multiplication table for numbers up to 12.

let rec print_table numbers step =
  match step
  | 1 -> printfn "%A" numbers
  | n -> printfn "%A" [ for i in numbers -> i * step ]; print_table numbers (step - 1)


[<EntryPoint>]
let main =
  let numbers = [ 1..12 ]

  print_table numbers 12
