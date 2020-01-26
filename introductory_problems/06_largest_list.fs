// A program to find the largest number in an unsorted list.

open System

[<EntryPoint>]
let main args =
  let test_list = [5;55;66;101;4;323;67;99;102;53;90;83;67;35;76]

  // Recursively find the largest value in the list
  let rec result big list =
    match list with
    | head :: tail ->
      let big = if head > big then head else big
      result big tail
    | [] ->  big

  let res = result test_list.Head test_list.Tail
  printfn "The largest value is %d" res

  0
