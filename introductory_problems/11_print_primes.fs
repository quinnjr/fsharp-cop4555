// A program that prints the first 30 prime numbers.

let is_prime a b =
  a = b || b % a <> 0

[<EntryPoint>]
let main args =
  let list = [ 1..30 ]
  for i of list do
    if is_prime list[i] list[i+1] then printf i else printf ""

  0
