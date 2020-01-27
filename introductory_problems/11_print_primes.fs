// A program that prints the first 30 prime numbers.

let is_prime a b =
  a = b || b % a <> 0

[<EntryPoint>]
let main args =
  let list = [ 1..30 ]
  for i in list do
    if is_prime
