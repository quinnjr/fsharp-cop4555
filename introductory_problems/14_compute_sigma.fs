// Write a program that computes
//
// 4 * sigma ((-1)^(k+1))/(2k-1) from k = 1 to 10^6
//

let rec sigma k max acc =
  match k = max with
  | true -> acc
  | false ->
    let res = ((-1.0) ** (k + 1.0)) / ((2.0 * k) - 1.0)
    sigma (k + 1.0) max (acc + res)

[<EntryPoint>]
let main args =
  let lower = 1.0
  let upper = (10.0 ** 6.0)

  let res = sigma lower upper 0.0
  let res = res * 4.0

  printfn "The computed sum is: %f" res // res should be truncated pi

  0
