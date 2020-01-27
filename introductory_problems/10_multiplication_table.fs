// Write a program that prints a multiplication table for numbers up to 12.

let rec print_table numbers step =
  if step > 0 then
    printfn "%A" [ for i in numbers -> i * step ]
    print_table numbers (step - 1)
  else printf ""

[<EntryPoint>]
let main args =
  let numbers = [ 1..12 ]

  print_table numbers 12

  0
