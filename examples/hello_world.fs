// Hello World example
//
// Simply prints "Hello World" using F#'s built-in `printfn` function.
// Adds a `main` function to the function call and a `printit` function
// with a string parameter to showcase Functional Programming style.

let printit str =
  printfn "%s" str

[<EntryPoint>]
let main =
  printit "Hello World"

  0
