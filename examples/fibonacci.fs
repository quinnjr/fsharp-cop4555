// General example of making a recursive function in F#
// using the fibonacci sequence.

let rec fibonacci i =
  match i < 2 with
  | true -> 1
  | false -> fibonacci(i-2) + fibonacci(i-1)

[<EntryPoint>]
let main args =
  printfn "%i" (fibonacci 2)
  printfn "%i" (fibonacci 6)
  printfn "%i" (fibonacci 11)
  printfn "%i" (fibonacci 20)

  0
