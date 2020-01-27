// A program that prints the first 30 prime numbers.

let is_prime n =
  let rec check i =
    i > n/2 || (n % i <> 0 && check (i + 1))
  check 2

[<EntryPoint>]
let main args =
  let list = [ 1..30 ]
  printf "The first 30 prime numbers are: "
  for i in list do
    match is_prime i with
    | false -> printf ""
    | true -> printf "%d " i

  0
