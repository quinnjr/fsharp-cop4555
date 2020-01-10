// General example of making a recursive function in F#
// using the fibonacci sequence.

let rec fibonacci i =
  if i < 2
  then 1
  else fibonacci(i-2) + fibonacci(i-1)

printfn "%i" (fibonacci 2)
printfn "%i" (fibonacci 6)
printfn "%i" (fibonacci 11)
printfn "%i" (fibonacci 20)
