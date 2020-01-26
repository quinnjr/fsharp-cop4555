// Check if an element exists in a list

let rec is_in_list elem list =
  match list with
  | [] -> false
  | tail::head when head = elem -> true
  | tail::head -> is_in_list elem tail

[<EntryPoint>]
let main args =
    let test_list = [5;55;66;101;4;323;67;99;102;53;90;83;67;35;76]

    printfn "Checking if the element 90 is in the test list"
    let res = is_in_list 90 test_list
    printfn "Is 90 in the list: %b" res

    printfn "Checking if the element 42 is in the test list"
    let res = is_in_list 42 test_list
    printfn "Is 42 in the list: %b" res

    0
