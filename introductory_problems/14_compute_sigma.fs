// Write a program that computes
//
// 4 * sigma ((-1)^(k+1))/(2k-1) from k = 1 to 10^6
//

let rec sigma k max acc =
  match k = max with
  | true -> acc
  | false ->
    let res = ((-1) ** (k + 1)) / ((2 * k) - 1)
    sigma (k + 1) max (acc + res)

[<EntryPoint>]
let main args =
  let start = 1.0
  let end = 10 ** 6 // Exponential power f only works on floating-point types

  let res = sigma start end 0
  let res = res * 4

  printfn "The computed sum is: %0f" res

  0
