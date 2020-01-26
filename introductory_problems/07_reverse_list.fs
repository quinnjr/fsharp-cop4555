// A program to reverse an input, unsorted list.

open System

let rec uno_reverse = function
| [] -> []
| [x] -> [x]
| head::tail -> uno_reverse tail @ [head]

[<EntryPoint>]
let main args =
  let test_list = [5;55;66;101;4;323;67;99;102;53;90;83;67;35;76]

  // Recursively build a new list of the reverse of the old list
  let res = uno_reverse test_list
  printfn "The initial list is %A" test_list
  printfn "The reversed list is %A" res

  0
